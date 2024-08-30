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

fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME: ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO: ")
(todo-me (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO(ashlin.eldridge): ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG: ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK: ")

emacs-lisp-mode inferior-emacs-lisp-mode

(declare-function "(declare-function " p " \"" q "\")")
(defconst "(defvar " p " " p n> "\"" p "\")")
(defun "(defun " p " (" p ")" n> "\"" p "\"" n> r> ")")
(defvar "(defvar " p " " p n> "\"" p "\")")
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
(sink-with-id "> (my/eshell-sink \"" p "\")")

org-mode

(begin "#+begin_" (s name) n> r> n "#+end_" name)
(begin-quote "#+begin_quote" n> r> n> "#+end_quote")
(begin-example "#+begin_example" n> r> n> "#+end_example")
(begin-comment "#+begin_comment" n> r> n> "#+end_comment")
(begin-src "#+begin_src " p n> q n> "#+end_src")

go-mode go-ts-mode

(const-block "const (" n> q n ")" >)
(ctx "ctx := context.Background()")
(ctx-cancel "ctx, cancel := context.WithCancel(context.Background())" n> "defer cancel()")
(fmt-printf "fmt.Printf(\"" p " %v\\n\", " q ")")
(fmt-println "fmt.Println(" q ")")
(for "for " p "{" n> q n "}" >)
(for-range "for " p ", " p " := range " p " {" n> q n "}" >)
(func "func " p "(" p ") " p " {" n> q n "}" >)
(if "if " p " {" n> r> n "}" >)
(if-else "if " p " {" n> p n "} else {" > n> q n "}" >)
(if-else-if "if " p " {" n> p n "} else if " > p " {" n> q n "}" >)
(if-else-if-else "if " p " {" n> p n "} else if " > p " {" n> p n "} else {" > n> q n "}" >)
(if-err "if err != nil {" n> q n "}" >)
(if-err-compact "if err := " p "; err != nil {" n> q n "}" >)
(if-err-propagate "if err != nil {" n> "return errors.Propagate(err)" n "}" >)
(import-block "import (" n> q n ")" >)
(init "func init() {" n> q n "}" >)
(interface "type " p " interface {" n> q n "}" >)
(main "func main() {" n> q n "}" >)
(map "map[" p "]" q)
(select "select {" n> "case " p ":" n> q n "}" >)
(struct "type " p " struct {" n> q n "}" >)
(switch "switch " p " {" n> "case " p ":" q n "}" >)
(test "func Test" p " (t *testing.T) { " n> q n "}" >)

rust-mode rust-ts-mode

(dbg "dbg!(" r> ")")
(derive "#[derive(" r> ")]")
(fn "fn " p "(" p ") {" n> r> n> "}")
(fn-return "fn " p "(" p ") -> " p " {" n> r> n> "}")
(if "if " p " {" n> r> n> "}")
(if-else "if " p " {" n> r> n> "} else {" n> p n> "}")
(if-else-if "if " p " {" n> r> n> "} else if " p " {" n> p n> "}")
(loop "loop {" n> r> n> "}")
(main "fn main() {" n> r> n> "}")
(main-tokio "#[tokio::main]" n> "async fn main() {" n> r> n> "}")
(new "pub fn new(" p ") -> Self {" n> "Self {}" n> "}" g)
(test "#[test]" n> "fn " p "() {" n> "assert_eq!(1 + 1, 2);" n> "}")
(test-mod "#[cfg(test)]" n> "mod tests {" n> "use super::*;" n n> "#[test]" n> "fn it_works() {" n> "assert_eq!(1 + 1, 2);" n> "}" n> "}")

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: "[a-z]"
;; End:
