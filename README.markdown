	Emacs config for Sean Fisk
==========================
	
This is my repository for Emacs. It is a fork of the excellent [Emacs Kicker](https://github.com/dimitri/emacs-kicker) by [Dimitri Fontaine](https://github.com/dimitri). As it's based on the Emacs Kicker, it also significantly leverages the might of Dimitri's powerhouse package-grabber for Emacs, [el-get](https://github.com/dimitri/el-get). Thanks Dimitri for both these awesome projects. My Emacs configuration is licensed under the terms of the [MIT License](http://www.opensource.org/licenses/mit-license.php).

Focus
-----

My Emacs configuration is geared toward the following purposes:

* Ruby on Rails coding
* C++ coding
* Bash and shell coding
* Elisp coding
* Writing of various text formats (Markdown, Textile)
	
These will be documented in more detail at a later time.

Compatibility
-------------

This Emacs config strives to be compatible with [GNU Emacs 23](http://www.gnu.org/software/emacs/) (stable) on GNU/Linux and [Aquamacs 2.x](http://aquamacs.org/) (based on GNU Emacs 23) on Mac OS X. However, I've just given back my loaner Mac so it will be hard for me to test on Aquamacs. Please submit issue reports if you have problems.

Install
-------

Before installing, make sure that required external packages are also installed if you'd like to use the associated el-get package. Listed are Apt packages for [Debian](http://www.debian.org/distrib/packages) and [Ubuntu](http://packages.ubuntu.com/), [Homebrew](https://github.com/mxcl/homebrew) packages for Mac OS X, [Ruby Gems](http://rubygems.org/), or other installation methods.

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
    <td><a href="http://cx4a.org/software/rsense/">rsense</a></td>
    <td></td>
    <td></td>
    <td></td>
    <td>Ruby 1.8.x, Java 1.5+ (<a href="http://cx4a.org/software/rsense/manual.html#Installation">See Rsense manual</a>)</td>
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
    <td><a href="http://jashkenas.github.com/coffee-script/">npm install coffee-script</a></td>
  </tr>
  <tr>
    <td><a href="https://github.com/senny/rvm.el">rvm</a></td>
    <td></td>
<td></td>
    <td></td>
    <td><a href="http://beginrescueend.com/">rvm</a></td>
  </tr>
  <tr>
    <td><a href="http://jblevins.org/projects/markdown-mode/">markdown-mode</a></td>
    <td>markdown (<a href="http://packages.debian.org/search?keywords=markdown">Debian</a>, <a href="http://packages.ubuntu.com/search?keywords=markdown">Ubuntu</a>)</td>
    <td><a href="https://github.com/mxcl/homebrew/blob/master/Library/Formula/markdown.rb">markdown</a></td>
    <td></td>
    <td>Only needed for translating Markdown to HTML</td>
</table>

I've been having trouble getting `magit` to work on Mac OS X. I've therefore created the `sean_no_magit` branch provide the same configuration without `magit`. Any input on this area is appreciated.
	
Here's how to install:

    # make sure ~/.emacs.d does not exist, this should replace it
    git clone git://github.com/seanfisk/emacs.git ~/.emacs.d
    emacs
		
Once Emacs has started, you will probably see a screen similar to this one:


This means `el-get` has been installed and is ready to start installing packages. Next, kill Emacs (`M-x kill-emacs`) and start it back up again.
	
Lots of packages should now begin to be installed. At some point, the package installation may stop and you may be show a screen like this:
	
	
Don't worry, this is not back. What you should now do is first find the init file with `C-x C-f ~/.emacs.d/init.el`. Once you have it open, you can now evaluate the init file without having to restart Emacs with `M-x eval-buffer`. If any further errors occur, simply run `eval-buffer` once again. It should all work out in the end, and you should see a screen similar to the following:

	
If the installation errors out permanently, feel free to submit an issue or contact me.

Customization
-------------

As soon as you clone the repo, you should create your own branch for customization. You will almost certainly want to customize your setup.

    bartholomew@compy:~/.emacs.d$ git checkout -b bartholomew

The file that you will probably want to edit is `init.el`. It should be well-commented.

Notes
-----
	
Following the Emacs Kicker, I've changed the default shortcut for `kill-emacs` of `C-x C-c` to `ido-switch-buffer`. I've reassigned `kill-emacs` to `C-x q` to keep an easy shortcut. Of course, you can always still close Emacs with `M-x kill-emacs`.

Credits
-------

Thanks to Dimitri Fontaine for creating the Emacs Kicker and el-get!

Thanks to my friends Siva and Paul for providing feedback on the config!
	
And of course, thanks to [GNU](http://www.gnu.org/) and [RMS](http://stallman.org/) for creating this awesome editor!
