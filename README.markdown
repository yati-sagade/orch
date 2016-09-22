orch
=====
Send scribbles and pictures from your Android phone to your Emacs org-mode
buffer. Current requirements:

- The computer running emacs and the phone running orch be on the same
  network. I'm working on a simple middleman server that both the app and emacs
  can talk to.

- emacs 24.3+

#### Setup

- Emacs
 - Put `orch.el` wherever you put your Emacs scripts (for me, this is
   ~/.emacs.d/vendor/).  Add the following to your `init.el` (or
   `.emacsrc`):

        (add-to-list 'load-path "~/.emacs.d/vendor")
        (autoload 'orch-toggle "orch" nil t)

 - At this point (after possibly restarting Emacs), doing `M-x
   orch-toggle` will start the orch server on port 4000 (Change the port if you
   want in `orch.el`, grep for 4000).

   ![Image showing Emacs after orch-toggle
   command](screenshots/orchserver-started.png)
  
 - Open a buffer in org-mode, if you haven't already (or just `M-x org-mode` in
   the current buffer).

- Android

 You can either install the app from the Play Store
 (https://play.google.com/store/apps/details?id=io.explog.orch) or build it
 yourself in Android Studio (import the `android/` directory as a project in
 AS).
 
    - Start the app, open settings, and fill in the IP address of your computer
      on which Emacs is running (the computer and the phone should be on the
      same network -- I'm working on making it work "via the cloud").
    - Hit the back button and test the connection by tapping "PING SERVER"
    on the action bar. If all is well, you should see a message saying
    "pong".

  <br/>
  <img alt="Image showing an Android toast with PONG as the text" width="200" height="355" src="screenshots/android-pong.png" />
 
### Using

Draw something on the Android app and hit the OK (✓) button.

<br/>
 <img alt="Image showing a figure in Orch" width="200" height="355" src="screenshots/android-figure.png" />

The emacs org mode buffer should now have a link to an image in it.

![Image highlighting a local image link in Emacs](screenshots/link-shown.png)

To see the image inline, do `C-c C-x C-v` -- this toggles the inline image view
in org mode.

![Image showing a picture in inline-mode in an org buffer in
Emacs](screenshots/picture-shown.png)

By default, the images land in ~/Pictures/orch/ (change this by editing `orch.el`).

