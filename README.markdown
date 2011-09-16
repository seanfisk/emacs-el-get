Emacs config for Sean Fisk
==========================
	
This is my repository for Emacs. It is a fork of the excellent [Emacs Kicker](https://github.com/dimitri/emacs-kicker) by [Dimitri Fontaine](https://github.com/dimitri). As it's based on the Emacs Kicker, it also significantly leverages the might of Dimitri's powerhouse package-grabber for Emacs, [el-get](https://github.com/dimitri/el-get). Thanks Dimitri for both these awesome projects. My Emacs configuration is licensed under the terms of the [MIT License](http://www.opensource.org/licenses/mit-license.php).
	
Focus
-----
	
My Emacs configuration is geared toward the following purposes:

* C++ coding	
* Shell scripting
* EmacsLisp coding
* Python coding
* Ruby coding
* Writing of various text formats (Markdown, Textile)
  
These will be documented in more detail at a later time.
	
Compatibility
-------------
	
This Emacs config strives to be compatible with [GNU Emacs 23](http://www.gnu.org/software/emacs/) (stable) on GNU/Linux and [Aquamacs 2.x](http://aquamacs.org/) (based on GNU Emacs 23) on Mac OS X. However, I no longer own a Mac so it will be hard for me to test on Aquamacs. Please submit issue reports if you have problems.

Dependencies
------------
	
Before installing, make sure that required external packages are also installed if you'd like to use the associated el-get package. Listed are Apt packages for [Debian](http://www.debian.org/distrib/packages) and [Ubuntu](http://packages.ubuntu.com/), [Homebrew](https://github.com/mxcl/homebrew) packages for Mac OS X, [Ruby Gems](http://rubygems.org/), [PyPi Packages](http://pypi.python.org/pypi) or other installation methods. I've tried to do dependency checking, so each package won't be installed unless its dependencies exist (i.e., `coffee-mode` will not get installed if the `coffee` executable is not found).

<table>
  <tr>
    <th>el-get</th>
    <th>apt</th>
    <th>homebrew</th>
    <th>gem</th>
    <th>pypi</th>
    <th>other</th>
  </tr>
  <tr>
    <td><a href="http://www.xsteve.at/prg/vc_svn/">psvn</a>, <a href="http://code.google.com/p/yasnippet/">yasnippet</a> (required)</td>
    <td>subversion (<a href="http://packages.debian.org/search?keywords=subversion">Debian</a>, <a href="http://packages.ubuntu.com/search?keywords=subversion">Ubuntu</a>)</td>
    <td><a href="https://github.com/mxcl/homebrew/blob/master/Library/Formula/subversion.rb">subversion</a></td>
    <td colspan="3"></td>
  </tr>
  <tr>
    <td><a href="http://www.emacswiki.org/emacs/Magit">magit</a> (required)</td>
    <td>texinfo (<a href="http://packages.debian.org/search?keywords=texinfo">Debian</a>, <a href="http://packages.ubuntu.com/search?keywords=texinfo">Ubuntu</a>)</td>
    <td><a href="https://github.com/mxcl/homebrew/blob/master/Library/Formula/texinfo.rb">texinfo</a></td>
    <td colspan="3"></td>
  </tr>
  <tr>
    <td><a href="http://www.emacswiki.org/emacs/FullAck">full-ack</a></td>
    <td>ack-grep (<a href="http://packages.debian.org/search?keywords=ack-grep">Debian</a>, <a href="http://packages.ubuntu.com/search?keywords=ack-grep">Ubuntu</a>)</td>
    <td><a href="https://github.com/mxcl/homebrew/blob/master/Library/Formula/ack.rb">ack</a></td>
    <td colspan="3"></td>
  </tr>
  <tr>
    <td><a href="http://rope.sourceforge.net/">Rope / Ropemacs (Python refactoring)</a></td>
    <td colspan="3"></td>
    <td><a href="http://pymacs.progiciels-bpi.ca/index.html">Pymacs</a>, <a href="http://pypi.python.org/pypi/rope">rope</a>, <a href="http://pypi.python.org/pypi/ropemode">ropemode</a>, <a href="http://pypi.python.org/pypi/ropemacs">ropemacs</a></td>
    <td></td>
  </tr>
  <tr>
    <td><a href="http://cx4a.org/software/rsense/">rsense</a></td>
    <td colspan="4"></td>
    <td>Ruby 1.8.x, Java 1.5+ (<a href="http://cx4a.org/software/rsense/manual.html#Installation">See Rsense manual</a>)</td>
  </tr>
  <tr>
    <td><a href="http://www.emacswiki.org/emacs/HamlMode">haml-mode</a></td>
    <td colspan="2"></td>
    <td><a href="http://rubygems.org/gems/haml">haml</a></td>
    <td colspan="2"></td>
  </tr>
  <tr>
    <td><a href="http://www.emacswiki.org/emacs/ScssMode">scss-mode</a></td>
    <td colspan="2"></td>
    <td><a href="http://rubygems.org/gems/sass">sass</a></td>
    <td colspan="2"></td>
  </tr>
  <tr>
    <td><a href="http://ozmm.org/posts/coffee_mode.html">coffee-mode</a></td>
    <td colspan="4"></td>
    <td><a href="http://jashkenas.github.com/coffee-script/">npm install coffee-script</a></td>
  </tr>
  <tr>
    <td><a href="https://github.com/senny/rvm.el">rvm</a></td>
    <td colspan="4"></td>
	<td><a href="http://beginrescueend.com/">rvm</a></td>
  </tr>
  <tr>
    <td><a href="http://jblevins.org/projects/markdown-mode/">markdown-mode</a></td>
    <td>markdown (<a href="http://packages.debian.org/search?keywords=markdown">Debian</a>, <a href="http://packages.ubuntu.com/search?keywords=markdown">Ubuntu</a>)</td>
    <td><a href="https://github.com/mxcl/homebrew/blob/master/Library/Formula/markdown.rb">markdown</a></td>
    <td colspan="2"></td>
    <td>Only needed for translating Markdown to HTML</td>
</tr>
</table>

* The beautiful [Inconsolata font](http://www.levien.com/type/myfonts/inconsolata.html) will be used if it is present. Otherwise, the default is Monaco on Mac and Monospace otherwise. To install Inconsolata on Debian/Ubuntu, simply install the `ttf-inconsolata` package or `levien-inconsolata-fonts` on Fedora. On Mac, I belive you must drag the [open type file](http://www.levien.com/type/myfonts/Inconsolata.otf) into your [fonts folder](http://support.apple.com/kb/HT2435).

* I've been having trouble getting `magit` to work on Mac OS X. I've therefore created the `sean_no_magit` branch provide the same configuration without `magit`. Any input on this area is appreciated.

Here's how to install:

1. Make sure `~/.emacs.d` does not exist, because this repository is intended to replace it.

       git clone git://github.com/seanfisk/emacs.git ~/.emacs.d
	   
1. Create your own branch for customization. You will almost certainly want to customize your setup.

	    bartholomew@compy:~/.emacs.d$ git checkout -b bartholomew
		
	If you do decide to customize, the file that you will probably want to edit is `init.el`. It should be well-commented.
	
1. Start Emacs with `emacs`. Once it has started, you will probably see a screen similar to this one:

	<a href="http://seanfisk.github.com/emacs/images/screenshots/1-el-get-install.png"><img alt="el-get install" src="http://seanfisk.github.com/emacs/images/screenshots/1-el-get-install.png" width="421" height="448"></a>
	
	This means `el-get` has been installed and is ready to start installing packages.
	
1. Next, kill Emacs (`M-x kill-emacs`) and start it back up again. Lots of packages should now begin to be installed. At some point, the package installation may stop and you may be show a screen like this:

	<a href="http://seanfisk.github.com/emacs/images/screenshots/2-end-of-buffer.png"><img alt="End of buffer" src="http://seanfisk.github.com/emacs/images/screenshots/2-end-of-buffer.png" width="421" height="448"></a>
	
1. Don't worry, this is not unexpected. What you should now do is find the init file with `C-x C-f ~/.emacs.d/init.el`:

	<a href="http://seanfisk.github.com/emacs/images/screenshots/3-find-file-init.png"><img alt="Find init file" src="http://seanfisk.github.com/emacs/images/screenshots/3-find-file-init.png" width="421" height="448"></a>
	
1. Once you have it open, you can now evaluate the init file without having to restart Emacs with `M-x eval-buffer`, like so:

	<a href="http://seanfisk.github.com/emacs/images/screenshots/4-eval-buffer-init.png"><img alt="Eval buffer init" src="http://seanfisk.github.com/emacs/images/screenshots/4-eval-buffer-init.png" width="421" height="448"></a>

1. If any further errors occur, simply run `eval-buffer` once again. You may have to restart a couple times as well. It should all work out in the end, and you should see a screen similar to the following:

	<a href="http://seanfisk.github.com/emacs/images/screenshots/5-done.png"><img alt="Done" src="http://seanfisk.github.com/emacs/images/screenshots/5-done.png" width="421" height="449"></a>
	
1. If the installation errors out permanently, please submit an issue or contact me.

Notes
-----

Following the Emacs Kicker, I've changed the default shortcut for `kill-emacs` of `C-x C-c` to `ido-switch-buffer`. I've reassigned `kill-emacs` to `C-x q` to keep an easy shortcut. Of course, you can always still close Emacs with `M-x kill-emacs`.

Credits
-------

Thanks to Dimitri Fontaine for creating the Emacs Kicker and el-get!

Thanks to my friends Siva, Paul, and Jared for providing feedback on the config!

And of course, thanks to [GNU](http://www.gnu.org/) and [RMS](http://stallman.org/) for creating this awesome editor!
