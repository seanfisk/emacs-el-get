Emacs config for Sean Fisk
==========================

This is my repository for Emacs. It is a fork of the excellent [Emacs Kicker](https://github.com/dimitri/emacs-kicker) by Dimitri Fontaine. As it's based on the Emacs Kicker, it also significantly leverages the might of Dimitri's powerhouse package-grabber for Emacs, [el-get](https://github.com/dimitri/el-get). Thanks Dimitri for both these awesome projects.

Focus
-----

My Emacs configuration is geared toward the following purposes:

* Ruby on Rails coding
* C++ coding
* bash and shell coding
* elisp coding (of course)
* writing of various text formats

These will be documented better later.

Compatibility
-------------

This emacs config strives to be compatible with GNU Emacs 23 and 24 on Linux and Aquamacs 2.x and 3.x (based on GNU Emacs 23 and 24, respectively) on Mac OS X.

Install
-------

I like a simpler install than Dimitri's Emacs Kicker...

    # make sure ~/.emacs.d does not exist, this should replace it
    git clone git://github.com/seanfisk/emacs.git ~/.emacs.d
    emacs
    
... It's not exactly that simple. After you start Emacs once, you may have to restart it a few times before all the packages take effect.

Customization
-------------

As soon as you clone the repo, you should create your own branch for customization. You will almost certainly want to customize your setup; it is the Emacs way.

    bartholomew@compy:~/.emacs.d$ git checkout -b bartholomew
    
The file that you need to edit is pretty much the only file in there, `init.el`. It should be well-commented.

Credits
-------

Thanks to Dimitri Fontaine for creating the Emacs Kicker and el-get!

Thanks to my friend Siva for feedback on the config!

And of course, thanks to GNU and RMS for creating this awesome editor!
