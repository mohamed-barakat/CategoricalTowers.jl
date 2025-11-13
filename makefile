install:
	julia -e 'using Pkg; \
		Pkg.develop(path = "ToolsForCategoricalTowers"); \
		Pkg.develop(path = "QuotientCategories"); \
		Pkg.develop(path = "FpCategories"); \
	'

gen:
	$(MAKE) -C ToolsForCategoricalTowers gen
	$(MAKE) -C QuotientCategories gen
	$(MAKE) -C FpCategories gen

gen-full:
	$(MAKE) -C ToolsForCategoricalTowers gen-full
	$(MAKE) -C QuotientCategories gen-full
	$(MAKE) -C FpCategories gen-full

test:
	$(MAKE) -C ToolsForCategoricalTowers test
	$(MAKE) -C QuotientCategories test
	$(MAKE) -C FpCategories test

git-commit:
	$(MAKE) -C ToolsForCategoricalTowers git-commit
	$(MAKE) -C QuotientCategories git-commit
	$(MAKE) -C FpCategories git-commit

update-subsplits:
	./dev/manually_update_subsplits.sh
