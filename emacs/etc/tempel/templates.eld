;; ~/.config/emacs/etc/tempel/templates.eld

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Template syntax (https://github.com/minad/tempel#template-syntax)
;;
;;     “string”  Inserts a string literal.;
;;     p         Inserts an unnamed placeholder field.
;;     n         Inserts a newline.
;;     >         Indents with indent-according-to-mode.
;;     r         Inserts the current region. If no region is active, quits the
;;               containing template when jumped to.
;;     r>        Acts like r, but indent region.
;;     n>        Inserts a newline and indents.
;;     &         Insert newline unless there is only white space between line
;;               start and point.
;;     %         Insert newline unless there is only white space between point
;;               and line end.
;;     o         Like % but leaves the point before newline.
;;     (s NAME)  Inserts a named field.
;;     (p PROMPT <NAME> <NOINSERT>)
;;               Insert an optionally named field with a prompt.
;;               The PROMPT is displayed directly in the buffer as default
;;               value.
;;               If NOINSERT is non-nil, no field is inserted. Then the mini
;;               buffer is used for prompting and the value is bound to NAME.
;;     (r PROMPT <NAME> <NOINSERT>)
;;               Insert region or act like (p ...).
;;     (r> PROMPT <NAME> <NOINSERT>)
;;               Act like (r ...), but indent region.
;;     (p FORM <NAME> <NOINSERT>)
;;               Like p described above, but FORM is evaluated.
;;     (FORM ...)
;;               Other Lisp forms are evaluated. Named fields are lexically
;;               bound.
;;     q         Quits the containing template when jumped to.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: Sometimes using Tempel templates in Tree-sitter modes can result in
;; odd indentation (typically of the closing brace). See this discussion for
;; how to handle: https://github.com/minad/tempel/discussions/125.
;;
;; Note 1: Use the form: (loop "loop {" n> r> n> "}" >) for correct indentation
;; in Tree-sitter modes. Both n> and r> are required to get correct indentation
;; when a region is select and unselected. The final > is required to correctly
;; indent the closing brace. The last n> is sometimes but not always required
;; to get correct indentation of the closing brace.
;;
;; Note 2: Use n> whenever the following line could be indented. If no indentation
;; is required, the major mode should prevent it, but without this the following
;; line won't be indented when it should be.

fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

prog-mode

(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG: ")
(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME: ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK: ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO: ")
(todo-me (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO(ashlin.eldridge): ")

emacs-lisp-mode inferior-emacs-lisp-mode

(declare-function "(declare-function " p " \"" q "\")")
(defconst "(defvar " p " " p n> "\"" q "\")")
(defun "(defun " p " (" p ")" n> "\"" p "\"" n> r> ")")
(defvar "(defvar " p " " p n> "\"" q "\")")
(dolist "(dolist (" p ")" n> r> ")")
(if-let "(if-let (" p ")" n> r> ")")
(if-let* "(if-let* (" p ")" n> r> ")")
(lambda "(lambda (" p ") " r> ")")
(let "(let (" p ")" n> r> ")")
(let* "(let* (" p ")" n> r> ")")
(pcase "(pcase " p n "(" q "))" >)
(use-package "(use-package " p q n> ":ensure" n> ":defines" n> ":functions" n> ":preface" n> ":if" n> ":no-require" n> ":after" n> ":custom" n> ":bind" n> ":hook" n> ":commands" n> ":init" n> ":defer" n> ":config" n> ")")
(when-let "(when-let (" p ")" n> r> ")")
(when-let* "(when-let* (" p ")" n> r> ")")

eshell-mode

(sink "> (my/eshell-sink)")
(sink-with-id "> (my/eshell-sink \"" q "\")")

org-mode

(begin "#+begin_" (s name) n> r> n> "#+end_" name)
(begin-comment "#+begin_comment" n> r> n> "#+end_comment")
(begin-example "#+begin_example" n> r> n> "#+end_example")
(begin-quote "#+begin_quote" n> r> n> "#+end_quote")
(begin-src "#+begin_src " p n> q n> "#+end_src")

go-mode go-ts-mode

(const-block "const (" n> q n> ")" >)
(ctx "ctx := context.Background()")
(ctx-cancel "ctx, cancel := context.WithCancel(context.Background())" n> "defer cancel()")
(for "for " p "{" n> r> n> "}" >)
(for-range "for " p ", " p " := range " p " {" n> r> n> "}" >)
(func "func " p "(" p ") " p " {" n> r> n> "}" >)
(if-err "if err != nil {" n> r> n> "}" >)
(if-err-compact "if err := " p "; err != nil {" n> r> n> "}" >)
(if-err-propagate "if err != nil {" n> "return errors.Propagate(err)" n> "}" >)
(import-block "import (" n> q n> ")" >)
(init "func init() {" n> q n> "}" >)
(interface "type " p " interface {" n> q n> "}" >)
(main "func main() {" n> q n> "}" >)
(map "map[" p "]" q)
(printf "fmt.Printf(\"" p " %v\\n\", " q ")")
(println "fmt.Println(" q ")")
(select "select {" n> "case " p ":" n> q n> "}" >)
(struct "type " p " struct {" n> q n> "}" >)
(switch "switch " p " {" n> "case " p ":" q n> "}" >)
(test "func Test" p "(t *testing.T) { " n> q n> "}" >)

rust-mode rust-ts-mode

(dbg "dbg!(" r ")")
(derive "#[derive(" q ")]")
(fn "fn " p "(" p ") {" q "}")
(fn-new "pub fn new(" p ") -> Self {" n> "Self {" q "}" n> "}" >)
(fn-return "fn " p "(" p ") -> " p " {" q "}")
(loop "loop {" n> r> n> "}" >)
(main "fn main() {" n> r> n> "}" >)
(main-tokio "#[tokio::main]" n> "async fn main() {" n> r> n> "}" >)
(print "print!(" p ")")
(println "println!(" p ")")
(test "#[test]" n> "fn " p "() {" q "}" >)
(test-mod "#[cfg(test)]" n> "mod tests {" n> "use super::*;" n n> "#[test]" n> "fn it_works() { assert_eq!(1 + 1, 2); }" n> "}" >)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: "[a-z]"
;; End:
