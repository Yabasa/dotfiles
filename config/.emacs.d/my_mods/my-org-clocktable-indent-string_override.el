(defun my-org-clocktable-indent-string (level)
  "Redefining this function to make the clock table
  more readable in the agenda view. Taken from:
  https://emacs.stackexchange.com/questions/9528/is-it-possible-to-remove-emsp-from-clock-report-but-preserve-indentation"
  (if (= level 1)
      ""
    (let ((str "."))
      (while (> level 2)
        (setq level (1- level)
              str (concat "." (concat str " "))))
      (concat str " "))))
	  
(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)
