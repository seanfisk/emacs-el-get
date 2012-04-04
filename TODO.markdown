TODO for Sean's Emacs Config
============================

Todo
----

* Use a shallow clone on some of the bigger git repos (nxhtml)
* Freeze as many packages as possible (most are already frozen)
* Get auto-complete 1.4 to work
* Re-do clike-minimal and python-minimal
* When filling code which uses tabs, alignment should still use spaces (may have
  to hack fillcode)
* Speed up auto-complete-ropemacs (may not be possible)

Packages to Check Out
---------------------

* ctags, ctags-update
* dtrt-indent
* flymake-shell (to replace my hand-rolled one)
* magit (through ELPA)
* smart-operator
* xclip

Done
----

* Remove unecessary packages
* Upgrade to the next version of el-get (or master branch) and make sure most,
  if not all of my packages are set to a stable version. This "so-and-so package
  doesn't work" is getting very annoying.
* Fix auto-complete, the application which spurred this.
* Code formatting
	* Follow Emacs Lisp style guide (on flymake-shell and flymake-python)
* Yasnippet
	* Learn and perfect it
* C++
	* Create a good C++ setup
	* Flymake (CMake flymake is in place)
	* Completion with auto-complete and/or semantic (cedet)
	* Preferably work well with STL and Qt
* Modularize the config
    * Split into different files which get required by init.el
* Emacs code browser (ECB) and cedet
    * Get ECB installed and working (current layout error is plaguing me)

Deferred
--------

* Rails
	* Get rails setup working properly
	* Document different modes / command
	* Add flymake-sass
	* Ensure ruby, erb, haml, sass, and coffee modes are working correctly

Removed
-------

* Em√acs goodies
	* Get this package working again
