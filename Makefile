SHELL         := /bin/zsh
EMACS_VERSION ?= 29

go_bin_dir    := ~/dev/go/bin
cargo_bin_dir := ~/.cargo/bin
apps_dir      := /Applications

no_color := \033[0m
ok_color := \033[38;5;74m

## Function for printing a pretty banner.
banner = \
	echo "\n$(ok_color)=====> $1$(no_color)"

## Function for checking that a variable is defined.
check_defined = \
	$(if $(value $1),,$(error Error: Variable $1 is required but undefined))

.PHONY: bootstrap
bootstrap: brew-install npm-install basic-init
	@$(call banner,Bootstrap complete)

.PHONY: basic-init
basic-init:
	@$(call banner,Performing basic initialisation procedure)
	@ln -sf ~/.config/zsh/lib/env.zsh ~/.zshenv
	@mkdir -p ~/.local/share/gnupg
	@ln -sf ~/.config/gnupg/gpg-agent.conf ~/.local/share/gnupg/gpg-agent.conf
# Not sure what the line below was ever meant to be doing?
#	@echo sudo dscl . -create /Users/$(USER) UserShell /opt/homebrew/bin/zsh
# Bazel doesn't follow XDG conventions and adding --bazelrc to the bz alias is not an
# option as it breaks completion.
	@ln -sf ~/.config/bazel/bazelrc ~/.bazelrc
	@echo Done.

.PHONY: dump-all
dump-all: brew-dump npm-dump go-dump cargo-dump apps-dump

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
	@npm list --location=global --depth=0 --json > npm/global-packages.json

.PHONY: npm-install
npm-install:
	@$(call banner,Installing NPM global packages)
	@for pkg in $$(jq -r '.dependencies | keys[]' < npm/global-packages.json); do \
		printf "\n===> Installing %s\n" "$${pkg}"; \
		npm install --location=global $$pkg; \
	done

.PHONY: go-dump
go-dump:
	@$(call banner,Dumping Go binaries)
	@mkdir -p go
	@ls -1 $(go_bin_dir) > go/bin.txt

.PHONY: cargo-dump
cargo-dump:
	@$(call banner,Dumping Cargo binaries)
	@mkdir -p cargo
	@ls -1 $(cargo_bin_dir) > cargo/bin.txt

.PHONY: apps-dump
apps-dump:
	@$(call banner,Dumping installed macOS applications)
	@mkdir -p mac
	@ls -1 $(apps_dir) > mac/apps.txt

.PHONY: emacs-install
emacs-install:
	@$(call banner,Installing Emacs version $(EMACS_VERSION))
	brew install emacs-plus@$(EMACS_VERSION) \
		--with-imagemagick \
		--with-no-frame-refocus \
		--with-poll \
		--with-native-comp \
		--with-savchenkovaleriy-big-sur-icon

.PHONY: emacs-uninstall
emacs-uninstall:
	@$(call banner,Uninstalling Emacs version $(EMACS_VERSION))
	@brew uninstall emacs-plus@$(EMACS_VERSION)
