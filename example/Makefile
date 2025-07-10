
# Source directory (one level up)
ghciPackageDir := ..

main = main
lhsExt = tex
texExt = tex

ghciPackageFiles := ghci.sty dkjson.lua
texBuild = build

all: latex

copy_files:
	@echo "copying the ghci package to example directory..."
ifeq ($(OS),Windows_NT)
	@for %%f in ($(ghciPackageFiles)) do $(copy_file) $(ghciPackageDir)\%%f . >nul
else
	$(copy_file) $(foreach file,$(ghciPackageFiles),$(ghciPackageDir)/$(file)) .
endif


latex: copy_files
	$(call mkdir_p,$(texBuild))
	lhs2TeX --poly -o $(texBuild)/$(main).$(texExt) $(main).$(lhsExt)
	latexmk -shell-escape  -file-line-error -lualatex -halt-on-error -outdir=$(texBuild) $(texBuild)/$(main).tex

clean:
 $(call rm_r,$(texBuild))


ifeq ($(OS),Windows_NT)
    copy_file = copy
else
    copy_file = cp
endif

ifeq ($(OS),Windows_NT)

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