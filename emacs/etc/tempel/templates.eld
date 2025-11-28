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
(todome (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO(ashlin.eldridge): ")

emacs-lisp-mode inferior-emacs-lisp-mode

(decfun "(declare-function " p " \"" q "\")")
(defconst "(defconst " p "\n  \"" p "\")")
(defun "(defun " p " (" p ")" n> "\"" p "\"" n> r> ")")
(defvar "(defvar " p " " p n> "\"" q "\")")
(dolist "(dolist (" p ")" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(iflett "(if-let* (" p ")" n> r> ")")
(int "(interactive" q ")")
(lambda "(lambda (" p ") " r> ")")
(let "(let (" p ")" n> r> ")")
(lett "(let* (" p ")" n> r> ")")
(pcase "(pcase " p n "(" q "))" >)
(usepackage "(use-package " p q n> ":ensure" n> ":defines" n> ":functions" n> ":preface" n> ":if" n> ":no-require" n> ":after" n> ":custom" n> ":bind" n> ":hook" n> ":commands" n> ":init" n> ":defer" n> ":config" n> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(whenlett "(when-let* (" p ")" n> r> ")")

eshell-mode

(sink "> (sink \"" (p "*eshell-output*") "\"" q ")")
(sinkyaml "> (sink-yaml \"" (p "*eshell-output.yaml*") "\"" q ")")
(sinkjson "> (sink-json \"" (p "*eshell-output.json*") "\"" q ")")

org-mode

(begin "#+begin_" (s name) n> r> n> "#+end_" name)
(begincomment "#+begin_comment" n> r> n> "#+end_comment")
(beginexample "#+begin_example" n> r> n> "#+end_example")
(beginquote "#+begin_quote" n> r> n> "#+end_quote")
(beginsrc "#+begin_src " p n> q n> "#+end_src")

go-mode go-ts-mode

(const "const (" n> q n> ")" >)
(ctx "ctx := context.Background()")
(ctxcancel "ctx, cancel := context.WithCancel(context.Background())" n> "defer cancel()")
(for "for " p "{" n> r> n> "}" >)
(forrange "for " p ", " p " := range " p " {" n> r> n> "}" >)
(func "func " p "(" p ") " p " {" n> r> n> "}" >)
(iferr "if err != nil {" n> r> n> "}" >)
(iferr2 "if err := " p "; err != nil {" n> r> n> "}" >)
(iferrprop "if err != nil {" n> "return errors.Propagate(err)" n> "}" >)
(import "import (" n> q n> ")" >)
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
(fnret "fn " p "(" p ") -> " p " {" q "}")
(new "pub fn new(" p ") -> Self {" n> "Self {" q "}" n> "}" >)
(loop "loop {" n> r> n> "}" >)
(main "fn main() {" n> r> n> "}" >)
(maintokio "#[tokio::main]" n> "async fn main() {" n> r> n> "}" >)
(print "print!(" q ")")
(println "println!(" q ")")
(test "#[test]" n> "fn " p "() {" q "}" >)
(testmod "#[cfg(test)]" n> "mod tests {" n> "use super::*;" n n> "#[test]" n> "fn it_works() { assert_eq!(1 + 1, 2); }" n> "}" >)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: "[a-z]"
;; End:
