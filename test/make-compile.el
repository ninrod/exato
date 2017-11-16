(setq files '("evil-textobj-xmlattr.el"))
(setq byte-compile--use-old-handlers nil)
(mapc #'byte-compile-file files)


