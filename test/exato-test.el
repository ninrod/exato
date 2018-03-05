(require 'ert)
(require 'evil)
(require 'evil-test-helpers)
(require 'exato)

(ert-deftest exato-test ()
  (ert-info ("basic evil move test")
    (evil-test-buffer
      :visual-start nil
      :visual-end nil
      "<a [c]lass=\"foo bar\">"
      ("l")
      "<a c[l]ass=\"foo bar\">"))
  (ert-info ("basic text")
    (evil-test-buffer
      :visual-start nil
      :visual-end nil
      "<a [c]lass=\"foo bar\">"
      ("dax")
      "<a>")))
