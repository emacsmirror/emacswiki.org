;;; epub-mode.el --- Modo menor para leer libros electronicos del formato epub en Emacs.

;; (Derechos reservados) Copyright (c) 2011 Jayson Williams 

;; Autor: Jayson Williams <williams.jayson@gmail.com>
;; Última actualización: 2011-10-26
;; versión: 0.1 (desarrollo temprano)
;; Palabra clave: epub
;; URL: https://sourceforge.net/projects/epubmode/
;; contribuidores
;;

;; epub-mode es un modo menor para ver documentos del formato epub en Emacs.

;; Este archivo es software libre. Puedes redistribuirlo y/o modificarlo
;; bajo los terminos de la Licencia Pública General de GNU como fue publicada por
;; Free Software Foundation (Fundación para el software libre) ya sea en la versión
;; dos, o (depende de usted) cualquier versión posterior.

;; Este archivo es distribuido con la esperanza de que será útil,
;; pero SIN NINGUNA GARANTIA. Sin siquiera la implicación de garantía de
;; COMERCIABILIDAD o APTITUD PARA UN PROPOSITO PARTICULAR. Vea la
;; Licencia Pública General de GNU para más información.

;; Usted debe haber recibido una copia de la Licencia Pública General de GNU
;; junto a su GNU Emacs. Vea el archivo COPYING. De no haberlo recibido escriba a
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth floor,
;; Boston, MA 02110-1301, USA.

;; Instalación

;; Durante el desarrollo temprano, instale por medio de epubmode.el y
;; evaluando el buffer.

;; Para usar epubmode use el comando:
;; M-x get_epub

;; Escriba la ruta del archivo epub y el modo epub abrirá una versión  
;; en forma de texto en Emacs.

(defun get_epub (epub_file)
  "Read epub files in emacs"
  (interactive "fname of epub: ")
  (message "%s" epub_file)
  (setq working_folder (concat epub_file "-working"))
  (setq mk_working_dir_cmd (concat "mkdir " working_folder))

  (if (file-directory-p working_folder)
      (message "working folder present")
    (progn
     (shell-command mk_working_dir_cmd)
     (setq unzip_epub_cmd (concat "unzip " epub_file " -d " working_folder))
     (shell-command unzip_epub_cmd)))

  (setq directories (directory-files working_folder nil "[^.]"))
  (setq directories (delete "META-INF" directories))
  (setq count (length directories))
  (setq index 0)
  ;;; crea html_files & book_txt despues de ordenar directorios existentes
  (shell-command (concat "mkdir " working_folder "/htm_files"))
  (shell-command (concat "mkdir " working_folder " /book_text"))

  (while (/= index count)
    ;;;necesita encontrar los archivos con los documentos html dentro
    (setq folder (elt directories index))
    (setq index (1+ index)) ;;; para la siguiente iteración
    (setq inner_folder_path (concat working_folder "/" folder))

    (if (file-directory-p inner_folder_path)
        (progn
          (message inner_folder_path)
          (sleep-for 2)
          (shell-command (concat "cp " inner_folder_path "/*.htm* " working_fol)
			 (message "not a folder")))))
  
  (shell-command (concat "touch " working_folder "/book.txt"))
  ;;; consigue el listado de htm-files , convierte a texto, colocar en book.txt
  (setq index 0)
  (setq htm_files (directory-files (concat working_folder "/htm_files") nil "[^.]"))
  (setq htm_files_count (length htm_files))
  ;;;(mensaje "%d archivos en htm_folder" htm_files_count)

  (while (/= index htm_files_count)
    (setq htm_file (elt htm_files index))
    (message "on htm_files %d: %s" index htm_file)
    (setq index (1+ index))
    (setq source (concat working_folder "/htm_files/" htm_file))
    (setq convert_to_txt (concat "html2text -ascii -nobs " source ">>" working_)
    (shell-command convert_to_txt)))
  (find-file (concat working_folder "/book.txt"))
)
