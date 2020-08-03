
�����ļ��н�ѹ����� org-page��λ��\theme �ļ����¼���

�� org-page ������վ��ļ򵥷�����

(require 'org-page)

(defun op/custom-org-page (&optional site)
  "choose the org-page's main repository"
  (interactive (let ((sitet (read-string "��̬վ������")))
                 (list sitet)))
  (cond         ;;emacs-china
        ((equal site "emacs-china")
         (setq op/repository-directory "~/github/emacs-china.github.io"
               op/site-domain "emacs-china.github.io"
               op/theme 'emacs_love
               op/personal-github-link "https://github.com/emacs-china"
               op/personal-disqus-shortname "emacs-china"
               op/personal-duoshuo-shortname "emacs-china"
               op/site-main-title "EMACS-CHINA"
               op/site-sub-title "=============>��˼����")
         )
                ;;xxxx
        ((equal site "xxxx")
         (setq op/repository-directory "xxxx"
               op/site-domain "xxxx"
               op/theme 'mdo
               op/personal-github-link "xxxx"
               op/personal-disqus-shortname "xxxx"
               op/personal-duoshuo-shortname "xxxx"
               op/site-main-title "xxxx"
               op/site-sub-title "xxxx")
         )
        (t nil)))
