mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))

# The following variables can be set in command line. For instance : "make run
# exe=myExe"

pkg = $(current_dir)
trg = lib
exe = ghci-server
tst = $(pkg)-test
bch = $(pkg)-bench

lhs = main
lhsExt = tex
texExt = tex
texBuild = build

stack = stack
stack-prof = $(stack) --work-dir $(stack-work-prof) --profile

stack-work = .stack-work
make-work  = .make-work
stack-work-prof = .stack-work-prof
# For explanation about why we need .stack-work-prof, see this stack issue
# https://github.com/commercialhaskell/stack/issues/4032

ifeq ($(OS),Windows_NT)
prof-ext = EXE.prof

define mkdir_p
	if not exist "$(1)" mkdir "$(1)"
endef

define move_file
	powershell -Command "Remove-Item -Path '$(2)' -ErrorAction SilentlyContinue; Move-Item -Path '$(1)' -Destination '$(2)'"
endef

define rm_r
	if exist "$(1)" powershell -Command "Remove-Item -Path '$(1)' -Recurse -Force"
endef

else # Non-Windows
prof-ext = prof

define mkdir_p
	mkdir -p $(1)
endef

define move_file
	rm -f "$(2)"; mv "$(1)" "$(2)"
endef

define rm_r
	( [ -d "$(1)" ] && rm -rf "$(1)" || true )
endef
endif

latex:
	$(call mkdir_p,$(texBuild))
	lhs2TeX --poly -o $(texBuild)/$(lhs).$(texExt) $(lhs).$(lhsExt)
	latexmk -shell-escape  -file-line-error -lualatex -halt-on-error -outdir=$(texBuild) $(texBuild)/$(lhs).tex


build:
	$(stack) build $(pkg):$(trg)

run:
	$(stack) build --fast $(pkg):exe:$(exe)  && $(stack) exec $(exe)

targets:
	$(stack) ide targets

clean:
	$(stack) clean

clean-hard:
	$(stack) clean
	$(call rm_r,$(make-work))
	$(call rm_r,$(stack-work))
	$(call rm_r,$(stack-work-prof))

doc :
	$(stack) haddock --no-haddock-deps

bench:
	$(call mkdir_p,$(make-work))
	$(stack) bench $(pkg):bench:$(bch) --ba "--output $(make-work)/$(bch).html"

install:
	$(stack) install $(pkg)


# Test

test:
	$(stack) test $(pkg):test:$(tst)

coverage:
	$(stack) test $(pkg):test:$(tst) --coverage



# Profiling

prof:
	$(call mkdir_p,$(make-work))
	$(stack-prof) build
	$(stack-prof) exec  -- $(exe) +RTS -p && $(call move_file,$(exe).$(prof-ext),./$(make-work)/$(exe).$(prof-ext))

prof-test:
	$(call mkdir_p,$(make-work))
	$(stack-prof) test $(pkg):test:$(tst) --test-arguments "+RTS -p" && mv $(tst).prof ./$(make-work)/$(tst).prof

prof-bench:
	$(call mkdir_p,$(make-work))
	$(stack-prof) bench $(pkg):bench:$(bch) --test-arguments "+RTS -p" && mv $(bch).prof ./$(make-work)/$(bch).prof

# GHCi

ghci:
	$(stack) ghci

ghci-exe:
	$(stack) ghci $(pkg):$(trg) --test --main-is $(pkg):$(exe)

ghci-test:
	$(stack) ghci $(pkg):test:$(tst)

ghci-bench:
	$(stack) ghci $(pkg):bench:$(bch)

# GHCid

ide:
	ghcid --command="stack ghci"

ide-exe:
	$(stack) exec -- ghcid -c "stack ghci $(pkg):$(trg) --test --main-is $(pkg):$(exe)"

ide-test:
	$(stack) exec -- ghcid -c "stack ghci $(pkg):test:$(tst) --test "

ide-bench:
	$(stack) exec -- ghcid -c "stack ghci $(pkg):bench:$(bch) --test"


.PHONY : build run targets clean clean-hard bench test coverage prof prof-test prof-bench ghci ghci-exe ghci-test ghci-bench ide ide-exe ide-test ide-bench
