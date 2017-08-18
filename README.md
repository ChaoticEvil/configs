### About this repositiry
This repository is a collection of the author's configuration files for some *nix-desktop programs, such as: emacs, dwm, st, cwm, tmux, mutt, urxvt.

### How to use it

	$ git clone https://github.com/ChaoticEvil/configs.git
	$ cp -r configs/. ~/
	
For using eslint with js2-mode in Emacs - install eslint globally:

	$ npm install -g eslint
	
### History

#### v0.6.0 [18.08.2017]
* Added: compton config (.compton.conf)
* Added: supplement script for inc/dec brightness (for cwm)
* Added: supplement script for inc/dec screen color with redshift (for cwm)
* Fixed: .muttrc
* Fixed: .Xdefaults
* Fixed: .perltidyrc
* Fixed: .perlcriticrc
* Fixed: .cwmrc (rewrite all config)
* Fixed: .rtorrent.rc
* Fixed: some refactoring in emacs config (X11 and terminal version)
* Fixed: .xinitrc (now using cwm by default window manager)
* Removed: unused fonts
* Upgraded: fonts (Iosevka v1.13.2, Fira Code v1.204, Fira Mono v3.206, Anonimous-pro, Progmata-pro, Consolas, Iconsolata, Hack v2.020, Go)

#### v0.5.6 [28.07.2017]
* Added: .mailcap
* Added: .rtorrent.rc
* Fixed: Multiple fixes (colors and external html viewer) in .muttrc

#### v0.5.5 [23.07.2017]
* Fixed: URxvt color and fonts in .Xdefaults

#### v0.5.4 [14.07.2017]
* Added: .muttrc
* Added: .sbt/

#### v0.5.3 [02.06.2017]
* Fixed: OS X meta for Emacs

#### v0.5.2 [02.06.2017]
* Fixes: Some improvements in Emacs config

#### v0.5.1 [09.04.2017]
* Removed: custom emacs themes directory
* Added: sample mc config - .config/mc/ini
* Added: sr-speedbar emacs plugin (filesystem tree side panel)

#### v0.5.0 [13.03.2017]
* Fixed: st config (with delete key)
* Fixed: dwm config (with volume change keys)
* Fixed: emacs config (refactoring)
* Fixed: zshrc config (some aliases)
* Added: scala influence in emacs config
* Added: simple black theme (based on hober-theme) in emacs
* Added: some fonts (fire mono, droind mono, m8+)
* Upgraded: fonts (Iosevka,monaco,progmata pro,etc)
* Removed: .wl (wanderlust) config
* Removed: sample web-servers configs
* Removed: some unused fonts

#### v0.4.2 [18.11.2016]
* Add GO-fonts (https://blog.golang.org/go-fonts) to the collection of fonts

#### v0.4.1 [22.10.2016]
* Minor edits in README.md

#### v0.4 [22.10.2016]
* Add tmux config file
* Upgrade Iosevka fonts (from 1.9.2 to 1.9.4)
* Add st (http://st.suckless.org/) config file
* Add dwm (http://dwm.suckless.org/) config file

#### v0.3 [11.08.2016]
* Some refactoring in emacs config.
* Separate emacs config in multiple small files.
* Upgrade Iosevka fonts (from 1.7.1 to 1.9.2)
* Add eslint config and emacs connections witn him.

#### v0.2 [18.01.2016]
* Update fonts collection.
* Few fixes in emacs config.
* Some fixes and changes in  README file.

#### v0.1 [20.10.2015]
* Wrote README file.
* Collection contains the best (according to the author) fonts for programming.
* Added basic, working version of the configuration files: emacs, zsh, cwm, Xdefaults and xinitrc.

<!-- EOF -->
