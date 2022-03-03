EMACS_VERSION ?= 28

no_color := \033[0m
ok_color := \033[38;5;74m

## Function for printing a pretty banner.
banner = \
	echo "\n$(ok_color)=====> $1$(no_color)"

## Function for checking that a variable is defined.
check_defined = \
	$(if $(value $1),,$(error Error: Variable $1 is required but undefined))

.PHONY: bootstrap
bootstrap: brew-install npm-install
	@$(call banner,Bootstrapping config)
	@ln -sf ~/.config/zsh/lib/env.zsh ~/.zshenv
	@ln -sf ~/.config/gnupg/gpg-agent.conf ~/.local/share/gnupg/gpg-agent.conf
	@chsh -s /usr/local/bin/zsh
	@echo Done.

.PHONY: brew-dump
brew-dump:
	@$(call banner,Dumping Brew packages)
	@cd homebrew; brew bundle dump -f

.PHONY: brew-install
brew-install:
	@$(call banner,Installing Brew packages)
	@cd homebrew; brew bundle

.PHONY: npm-dump
npm-dump:
	@$(call banner,Dumping NPM global packages)
	@npm list -g --depth=0 --json > npm/global-packages.json

.PHONY: npm-install
npm-install:
	@$(call banner,Installing NPM global packages)
	@for pkg in $$(jq -r '.dependencies | keys[]' < npm/global-packages.json); do \
		printf "\n===> Installing %s\n" "$${pkg}"; \
		npm install -g $$pkg; \
	done

.PHONY: uninstall-emacs
uninstall-emacs:
	@$(call banner,Uninstalling Emacs version $(EMACS_VERSION))
	@brew uninstall emacs-plus@$(EMACS_VERSION)

.PHONY: install-emacs
install-emacs:
	@$(call banner,Installing Emacs version $(EMACS_VERSION))
	@brew install emacs-plus@$(EMACS_VERSION) \
		--with-native-comp \
		--with-modern-black-gnu-head-icon
