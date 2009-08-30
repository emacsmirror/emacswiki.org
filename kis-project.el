;;; kis-project.el --- Extensions for kis-mode.
;;
;; Filename: kis-project.el
;; ------------------------------------------------------------------------------
;; $Author: ffrances $
;; $Date: 2006/02/14 00:10:19 $
;; $Revision: 1.3 $
;; $Id: kis-project.el,v 1.3 2006/02/14 00:10:19 ffrances Exp $
;; ------------------------------------------------------------------------------
;; 
;; This is not part of Gnu emacs.
;; This is part of kis-mode.
;;
;;  Copyright (C) 2005  Frederic Frances (frances _ frederic at yahoo dot fr)
;;
;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; see the file COPYING . If not, write to the
;;  Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; ------------------------------------------------------------------------------
;;
;; provide kis-project
;;
;; This file contains the customizable variable  `kis-project-action-list'
;; which is used by `kis-interface' for frame and custom command.
;;
;; kis-mode was developped on Gnu Emacs (21.3) it probably need some adaptation
;; for others version of emacs (xemacs,...)
;; ------------------------------------------------------------------------------
;;
;; $Log: kis-project.el,v $
;; Revision 1.3  2006/02/14 00:10:19  ffrances
;; add comment about emacs and xemacs.
;;
;; Revision 1.2  2006/02/13 23:47:38  ffrances
;; Add GPL.
;;
;;
;; ------------------------------------------------------------------------------

(defgroup kis-project nil
  "Sub group of `kis-mode' for project description"
  :group 'kis-mode)

(defcustom kis-project-action-list
  (list
   '("HOMELIST"  "<NOCOPY>" "<NOCOPY>" "<NOCOPY>" "cd $HOME && echo $HOME && ls" "Monitoring tools" "remote home directory listing")
   '("DiskUsage" "<NOCOPY>" "<NOCOPY>" "<NOCOPY>" "cd .. && du -ak" "Monitoring tools" "Read disk usage")
   '("DiskFree"  "<NOCOPY>" "<NOCOPY>" "<NOCOPY>" "df -ak" "Monitoring tools" "Read disk space")
   '("Ipc"       "<NOCOPY>" "<NOCOPY>" "<NOCOPY>" "ipcs -a" "Monitoring tools" "Read IPC information")
   '("Engine1" "myEngine1.kab" "${NODE_HOME}/design/build" "${NODE_HOME}/Engine1_Install_PATH/bin" "${NODE_HOME}/Tools/bin/installEngine.sh Engine1" "Engine Installation" "Install Engine1 from build directory to Installation directory")
   '("Engine2							  " "myEngine2.kab						  " "${NODE_HOME}/design/build					  " "${NODE_HOME}/Engine2_Install_PATH/bin			  " "${NODE_HOME}/Tools/bin/installEngine.sh Engine2		  " "Engine Installation						  " "Install Engine2 from build directory to Installation directory")
   '("Engine3							  " "myEngine3.kab						  " "${NODE_HOME}/design/build					  " "${NODE_HOME}/Engine3_Install_PATH/bin			  " "${NODE_HOME}/Tools/bin/installEngine.sh Engine3		  " "Engine Installation						  " "Install Engine3 from build directory to Installation directory")
   '("ECC" "<NOCOPY>" "<NOCOPY>" "<NOCOPY>" "swecc -P ${COORDINATORPORT}" "Graphical Kabira   Interface (use X)" "Invoke kabira swecc")
   '("MON" "<NOCOPY>" "<NOCOPY>" "<NOCOPY>" "cd ${NODE_HOME}/runtime && swmon ossm" "Graphical Kabira Interface (use X)" "Invoke swmon")
   '("Regedit" "<NOCOPY>" "<NOCOPY>" "<NOCOPY>" "cd ${NODE_HOME}/runtime && swregedit &" "Graphical Kabira Interface (use X)" "Invoke swregedit")
   '("DumpRegistry" "<NOCOPY>" "<NOCOPY>" "<NOCOPY>" "cd ${NODE_HOME}/runtime && swregistry -pe" "Monitoring tools" "Dump Registry")
   '("StopNode" "<NOCOPY>" "<NOCOPY>" "<NOCOPY>" "${NODE_HOME}/Tools/bin/stopNode.sh" "Monitoring tools" "Start kabira Node")
   '("StartNode" "<NOCOPY>" "<NOCOPY>" "<NOCOPY>" "${NODE_HOME}/Tools/bin/startNode.sh" "Monitoring tools" "Start kabira Node")
   '("KillMe" "<NOCOPY>" "<NOCOPY>" "<NOCOPY>" "kill -9 -1" "Monitoring Tools" "Kill all remote process (kill -9 -1)"))
 
"List possible custom command:
this list is used by `kis-interface-do-project-action' and `kis-interface-create-frame'

Fields description:

 - Action         : Access key for invoking command            (sample: Install Engine1)

 - Component name : Indicate the name of engine to install     (sample: Engine1.kab)
 - Build Path     : Build directory of the engine              (sample: $NODE_HOME/design/build)
 - Install Path   : Installation directory                     (sample: $NODE_HOME/Engine1/bin)

 - Command        : Command to invoke to finalize the command  (sample: swnode load deployEngine1.kds)

 - Categery       : Field used by `kis-interface-create-frame' to sort buttons
 - Comment        : Field used by `kis-interface-create-frame' to display a reminder note on minibuffer.

`kis-interface-do-project-action' perform the folowing action on remote host `kis-interface-host':
 - Copy engine \"Component name\" from \"Build path\" to \"Install path\"
 - print checksum of \"Component name\" in \"Build path\" and \"Install path\"
 - Invoke shell command \"Command\".

Note:

if \"Component name\" or \"Build path\" or \"Install path\" is set to \"<NOCOPY>\" then 
`kis-interface-do-project-action' only invoke the \"Command\""

  :type '(repeat (list (string :tag "Action        ")
		       (string :tag "Component name")
		       (string :tag "Build Path    ")
		       (string :tag "Install Path  ")
		       (string :tag "Command       ")
                       (string :tag "Category      ")
                       (string :tag "Comment       ")))
  :group 'kis-project
  )

(defun kis-project-customize ()
  "Customization of kis-project"
  (interactive)
  (customize-group 'kis-project))

;; ------------------------------------------------------------------------------
;; provided element for loading this library.
;; ------------------------------------------------------------------------------
(provide 'kis-project)
;; ------------------------------------------------------------------------------
;; End of file.
;; ------------------------------------------------------------------------------
