Emacs config for Sean Fisk
==========================

This is my repository for Emacs. It is a fork of the excellent [Emacs Kicker](https://github.com/dimitri/emacs-kicker) by Dimitri Fontaine. As it's based on the Emacs Kicker, it also significantly leverages the might of Dimitri's powerhouse package-grabber for Emacs, [el-get](https://github.com/dimitri/el-get). Thanks Dimitri for both these awesome projects. It is licensed under the terms of the [MIT License](http://www.opensource.org/licenses/mit-license.php).

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

This Emacs config strives to be compatible with GNU Emacs 23 (stable) on Linux and Aquamacs 2.x (based on GNU Emacs 23) on Mac OS X.

Install
-------

Before installing, make sure that required external packages are also installed if you'd like to use the associated el-get package. Listed are Apt packages for Debian and Ubuntu, Homebrew packages for Mac OS X, Ruby Gems, or other installation methods.

<table>
  <tr>
    <th>el-get</th>
    <th>apt</th>
    <th>homebrew</th>
    <th>gem</th>
    <th>other</th>
  </tr>
  <tr>
    <td><a href="http://www.emacswiki.org/emacs/FullAck">full-ack</a></td>
    <td>ack-grep (<a href="http://packages.debian.org/search?keywords=ack-grep">Debian</a>, <a href="http://packages.ubuntu.com/search?keywords=ack-grep">Ubuntu</a>)</td>
    <td><a href="https://github.com/mxcl/homebrew/blob/master/Library/Formula/ack.rb">ack</a></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td><a href="http://www.emacswiki.org/emacs/Magit">magit</a></td>
    <td>texinfo (<a href="http://packages.debian.org/search?keywords=texinfo">Debian</a>, <a href="http://packages.ubuntu.com/search?keywords=texinfo">Ubuntu</a>)</td>
    <td><a href="https://github.com/mxcl/homebrew/blob/master/Library/Formula/texinfo.rb">texinfo</a></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td><a href="http://www.emacswiki.org/emacs/HamlMode">haml-mode</a></td>
    <td></td>
    <td></td>
    <td><a href="http://rubygems.org/gems/haml">haml</a></td>
    <td></td>
  </tr>
  <tr>
    <td><a href="http://www.emacswiki.org/emacs/ScssMode">scss-mode</a></td>
    <td></td>
    <td></td>
    <td><a href="http://rubygems.org/gems/sass">sass</a></td>
    <td></td>
  </tr>
  <tr>
    <td><a href="http://ozmm.org/posts/coffee_mode.html">coffee-mode</a></td>
    <td></td>
    <td></td>
    <td></td>
    <td><a href="http://jashkenas.github.com/coffee-script/">nmp install coffee-script</a></td>
  </tr>
  <tr>
    <td><a href="https://github.com/senny/rvm.el">rvm</a></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td><a href="http://beginrescueend.com/">rvm</a></td>
  </tr>
</table>

I like a simpler install than Dimitri's Emacs Kicker:

    # make sure ~/.emacs.d does not exist, this should replace it
    git clone git://github.com/seanfisk/emacs.git ~/.emacs.d
    emacs
    
It's not exactly that simple. After you start Emacs once, you may have to restart it a few times before all the packages take effect. If you see errors, don't be worried, just restart Emacs a few times and it should work out. If it doesn't, feel free to contact me.

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
