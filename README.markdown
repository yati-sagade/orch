### orch

#### Setup

- Emacs
 - Put `orch.el` wherever you put your Emacs scripts (for me, this is
   ~/.emacs.d/vendor/).  Add the following to your `init.el` (or
   `.emacsrc`):

       (add-to-list 'load-path "~/.emacs.d/vendor")
       (autoload 'orch-toggle "orch" nil t)

 - At this point (after possibly restarting Emacs), doing `M-x
   orch-toggle` will start the orch server on port 4000.
  
 - Open a file in org-mode.
  

- Android
 - Build the Android app in `android/` in Android Studio.
 - Install the app on your phone.
 - Start the app and fill type the IP address of your computer on
   which Emacs is running (the computer and the phone should be
   on the same network -- I'm working on making it work via the
   cloud).
 - Hit the back button and test the connection by tapping "PING SERVER"
   on the action bar. If all is well, you should see a message saying
   "pong".
 
### Using

Draw figures on the Android app and hit the OK button. The emacs org
mode buffer should now have a link to an image in it. To see the image
inline, do `C-c C-x C-v` -- this toggles the inline image view in org
mode.

