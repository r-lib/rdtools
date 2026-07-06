// Fast Rd alias indexing.
//
// == Why this exists =========================================================
//
// pkgload::build_topic_index() originally called tools::parse_Rd() on every
// Rd file to extract its \alias{} entries. parse_Rd() builds a full parse
// tree of the document; we only need a handful of lines. Measured on
// ggplot2's man/ directory (228 Rd files, ~1.5MB, APFS, M-series Mac,
// warm file cache), building the whole index:
//
//   tools::parse_Rd() per file             ~324 ms
//   R regex (readLines + grepl + regexec)   ~26 ms   (13x)
//   this file, inverting via tapply()        ~7 ms   (45x)
//   this file, alias->file environment       ~5 ms   (66x)
//
// The original line-anchored scanner was verified byte-identical to the
// parse_Rd() version across 152 real package checkouts; the current scanner
// (relaxed to allow multiple aliases anywhere on a line) was re-verified
// against parse_Rd() on 435 Rd files across 11 local checkouts.
//
// == Where the time actually goes ============================================
//
// The final ~7 ms breaks down as:
//
//   ~6.1 ms  alias extraction — 26 us/file, which is essentially the cost
//            of open()/read()/close() per file. This is the I/O floor: no
//            language choice reduces it further (short of io_uring-style
//            batching or parallel reads, which are absurd at this scale).
//   ~0.9 ms  inverting file->aliases into alias->file in R via tapply().
//            A loop assigning into a new.env(hash = TRUE) does the same
//            job in ~160 us AND gives O(1) lookups — see "Lookup" below.
//   ~0.1 ms  directory listing (c_rd_files below).
//
// Lessons that transfer beyond this file:
//
// 1. READ THE WHOLE FILE AT ONCE. An fseek/ftell/fread of the entire file
//    into one buffer, then scanning lines in place, beat C++ getline()
//    line-by-line by ~30% (6.3 ms vs 8.8 ms for the full pipeline). One
//    syscall per file instead of many, zero per-line allocation.
//
// 2. R's dir(full.names = TRUE) IS SURPRISINGLY EXPENSIVE: ~550 us for one
//    228-entry directory, of which ~500 us is sorting. list.files() always
//    sorts, the sort uses locale collation (ICU/strcoll under any non-"C"
//    LC_COLLATE, including "C.UTF-8"), and with full.names every comparison
//    re-walks the long identical directory prefix before reaching a byte
//    that differs. Sorting the same strings under C collation costs ~110 us;
//    the raw readdir work is only ~50 us. If you must use dir() in a hot
//    path, list basenames and paste the prefix on afterwards.
//
// 3. WE DON'T SORT AT ALL HERE. readdir order is arbitrary and
//    filesystem-specific, so the R wrapper sorts basenames bytewise
//    (order(method = "radix"), ~10 us) for deterministic output; just never
//    sort full paths under locale collation (see point 2). Duplicated
//    aliases are a packaging mistake that the R side surfaces as a warning
//    rather than silently resolving.
//
// == Lookup: build an environment, not a list ================================
//
// The consumer (roxygen2 especially) performs MANY topic lookups against
// the index. Store alias -> file in a hashed environment:
//
//   env <- new.env(parent = emptyenv(), hash = TRUE, size = 2048L)
//   for each file: for each alias: assign(alias, file, envir = env)
//     (or first check exists(alias, env) and warn/error on duplicates)
//
// Measured: building the env is ~160 us (vs ~830 us for tapply-based
// inversion) and a single lookup is ~41 ns vs ~1.6 us for
// `topic %in% names(index)` + `index[[topic]]` on a 726-alias list — a 40x
// difference that compounds over roxygen2's thousands of cross-reference
// checks. Two footguns to remember with environments: ls() and as.list()
// default to all.names = FALSE and SILENTLY DROP keys starting with "."
// (ggplot2 really exports .data, .pt, .stroke, ...), and environments have
// reference semantics, so hand copies to callers with
// as.list(env, all.names = TRUE) if mutation is a concern.
//
// == GC safety ===============================================================
//
// Both functions collect results in R_alloc() scratch space (freed
// automatically by R when .Call returns) and only create CHARSXPs after the
// output STRSXP is allocated and PROTECTed, assigning each CHARSXP into the
// vector immediately. This matters: an earlier draft stored freshly created
// CHARSXPs in a C array while continuing to allocate, and any of those
// later allocations could trigger a GC that collects the unprotected
// strings. Buffering bytes, not SEXPs, sidesteps the whole class of bug.

#include <R.h>
#include <Rinternals.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>

// --- c_rd_files -------------------------------------------------------------
// Non-hidden *.Rd / *.rd files in `dirpath`, full paths, basenames as names.
// Returned in readdir order (see note 3 above). ~60-120 us for 228 files,
// vs ~550 us for dir(pattern = "\\.[Rr]d$", full.names = TRUE): the win is
// (a) no locale-collated sort and (b) a suffix check instead of a regex.

static int is_rd(const char* name) {
  if (name[0] == '.') return 0;  // matches dir()'s all.files = FALSE default
  size_t len = strlen(name);
  if (len < 3) return 0;
  return name[len - 3] == '.' &&
         (name[len - 2] == 'R' || name[len - 2] == 'r') &&
         name[len - 1] == 'd';
}

SEXP c_rd_files(SEXP dirpath) {
  const char* dpath = CHAR(STRING_ELT(dirpath, 0));
  DIR* d = opendir(dpath);
  if (!d) return Rf_allocVector(STRSXP, 0);

  // Growable array of basenames. R_alloc has no realloc, so grow by
  // allocate-and-copy; the transient blocks are reclaimed at .Call exit.
  int cap = 64, n = 0;
  const char** names = (const char**) R_alloc(cap, sizeof(char*));

  struct dirent* e;
  while ((e = readdir(d)) != NULL) {
    if (!is_rd(e->d_name)) continue;
    if (n == cap) {
      const char** bigger = (const char**) R_alloc(cap * 2, sizeof(char*));
      memcpy(bigger, names, cap * sizeof(char*));
      names = bigger;
      cap *= 2;
    }
    size_t len = strlen(e->d_name);
    char* copy = R_alloc(len + 1, 1);
    memcpy(copy, e->d_name, len + 1);
    names[n++] = copy;
  }
  closedir(d);

  // Shared path buffer: prefix written once, basename overwritten per file.
  // d_name is capped at NAME_MAX (255) on macOS/Linux, so 256 is safe.
  size_t dlen = strlen(dpath);
  char* full = R_alloc(dlen + 1 + 256 + 1, 1);
  memcpy(full, dpath, dlen);
  full[dlen] = '/';

  SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, n));
  for (int i = 0; i < n; i++) {
    size_t len = strlen(names[i]);
    if (len > 256) len = 256;
    memcpy(full + dlen + 1, names[i], len);
    SET_STRING_ELT(out, i, Rf_mkCharLenCE(full, (int)(dlen + 1 + len), CE_UTF8));
    SET_STRING_ELT(nms, i, Rf_mkCharCE(names[i], CE_UTF8));
  }
  Rf_setAttrib(out, R_NamesSymbol, nms);
  UNPROTECT(2);
  return out;
}

// --- c_rd_aliases -----------------------------------------------------------
// Extract \alias{...} entries from one Rd file, by scanning lines:
//   * an unescaped % starts a comment running to end of line
//   * any number of \alias{} commands may appear on a line, anywhere in
//     the line, but an alias must open and close on the same line
//   * \% \{ \} \\ are unescaped in the result; other escape sequences and
//     balanced inner braces pass through verbatim
//   * \\alias{} (escaped backslash) is text, not an alias
//
// Two things a line scanner cannot see: aliases produced by Rd macros, and
// (false positive, not miss) a literal \alias{} inside a verbatim block
// like \examples{} — both need a full parse and neither occurs in practice.
//
// ~26 us/file, of which nearly all is fopen/fread/fclose. The scan itself
// touches each byte at most twice (comment scan + copy) with no allocation.

typedef struct {
  size_t* offset;
  long* length;
  int n;
  int cap;
  char* scratch;  // unescaped aliases, written contiguously
  size_t used;
} alias_buf;

static void alias_push(alias_buf* b, long len) {
  if (b->n == b->cap) {
    size_t* off2 = (size_t*) R_alloc(b->cap * 2, sizeof(size_t));
    long* len2 = (long*) R_alloc(b->cap * 2, sizeof(long));
    memcpy(off2, b->offset, b->cap * sizeof(size_t));
    memcpy(len2, b->length, b->cap * sizeof(long));
    b->offset = off2;
    b->length = len2;
    b->cap *= 2;
  }
  b->offset[b->n] = b->used;
  b->length[b->n] = len;
  b->used += len;
  b->n++;
}

// Scan one line, appending any aliases found to `b`.
static void scan_line(const char* line, size_t len, alias_buf* b) {
  size_t i = 0;
  while (i < len) {
    char c = line[i];
    if (c == '%') return;  // comment runs to end of line

    if (c != '\\') {
      i++;
      continue;
    }

    if (len - i >= 7 && strncmp(line + i, "\\alias{", 7) == 0) {
      // Copy the argument into scratch, unescaping as we go and tracking
      // brace depth so balanced inner braces don't end the alias early.
      char* out = b->scratch + b->used;
      size_t j = i + 7;
      long n = 0;
      int depth = 1;
      while (j < len) {
        char d = line[j];
        if (d == '\\' && j + 1 < len) {
          char nx = line[j + 1];
          if (nx == '%' || nx == '{' || nx == '}' || nx == '\\') {
            out[n++] = nx;
            j += 2;
            continue;
          }
          out[n++] = d;
          j++;
          continue;
        }
        if (d == '%') return;  // comment: this alias never closes
        if (d == '{') depth++;
        if (d == '}' && --depth == 0) break;
        out[n++] = d;
        j++;
      }
      if (j == len) return;  // unterminated: multi-line aliases unsupported
      alias_push(b, n);
      i = j + 1;
      continue;
    }

    i += 2;  // escape pair (\x), or a lone trailing backslash
  }
}

SEXP c_rd_aliases(SEXP path) {
  const char* p = CHAR(STRING_ELT(path, 0));
  FILE* f = fopen(p, "rb");
  if (!f) Rf_error("Can't open '%s'", p);

  // One read for the whole file — see note 1 in the header comment.
  fseek(f, 0, SEEK_END);
  long size = ftell(f);
  fseek(f, 0, SEEK_SET);
  char* buf = R_alloc(size + 1, 1);
  size_t got = fread(buf, 1, size, f);
  fclose(f);
  buf[got] = '\0';

  // Unescaped aliases are written contiguously into `scratch` (their total
  // length can't exceed the file size), with offsets/lengths recorded, so
  // CHARSXP creation can wait until the output vector exists (GC safety —
  // see header comment).
  alias_buf b;
  b.cap = 32;
  b.n = 0;
  b.offset = (size_t*) R_alloc(b.cap, sizeof(size_t));
  b.length = (long*) R_alloc(b.cap, sizeof(long));
  b.scratch = R_alloc(got + 1, 1);
  b.used = 0;

  char* line = buf;
  while (line < buf + got) {
    char* nl = memchr(line, '\n', buf + got - line);
    size_t len = nl ? (size_t)(nl - line) : (size_t)(buf + got - line);
    if (len > 0 && line[len - 1] == '\r') len--;

    scan_line(line, len, &b);

    if (!nl) break;
    line = nl + 1;
  }

  SEXP out = PROTECT(Rf_allocVector(STRSXP, b.n));
  for (int i = 0; i < b.n; i++) {
    SET_STRING_ELT(out, i,
                   Rf_mkCharLenCE(b.scratch + b.offset[i], (int)b.length[i],
                                  CE_UTF8));
  }
  UNPROTECT(1);
  return out;
}
