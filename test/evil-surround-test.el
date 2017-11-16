(require 'ert)
(require 'evil)
(require 'evil-test-helpers)
(require 'evil-textobj-xmlattr)

(ert-deftest evil-textobj-xml-attr-test ()
  (ert-info ("basic evil move test")
    (evil-test-buffer
      "<a [c]lass=\"foo bar\""
      ("l")
      "<a c[l]ass=\"foo bar\""))
  (ert-info ("basic text")
    (evil-test-buffer
      "<a [c]lass=\"foo bar\""
      ("dax")
      "<a")))
