no_color := \033[0m
ok_color := \033[38;5;74m

# Function for printing a pretty banner
banner = \
	echo "\n$(ok_color)=====> $1$(no_color)"

# Function for checking that a variable is defined
check_defined = \
	$(if $(value $1),,$(error Error: Variable $1 ($2) is undefined))

.PHONY: brew-install
brew-install:
	@$(call banner,Installing Brew packages)
	@cd homebrew; brew bundle

.PHONY: brew-dump
brew-dump:
	@$(call banner,Dumping Brew packages)
	@cd homebrew; brew bundle dump -f

.PHONY: update-submodules
update-submodules:
	git submodule update --init --recursive
	git submodule update --remote
	@cd nvim/bundle/coc.nvim && git checkout release && git reset --hard origin/release
	git submodule foreach 'git pull --recurse-submodules origin `git rev-parse --abbrev-ref HEAD`'

## Removes a git submodule (e.g., MODULE=nvim/bundle/nginx.vim).
.PHONY: remove-submodule
remove-submodule:
	@$(call check_defined, MODULE)
	mv $(MODULE) $(MODULE).tmp
	git submodule deinit -f -- $(MODULE)
	rm -rf .git/modules/$(MODULE)
	git rm -f $(MODULE)
	rm -rf $(MODULE).tmp
