#!/bin/bash

set -e

# Define GitHub username
if [ -z "$1" ]; then
  GITHUB_USER="homalg-project"
else
  GITHUB_USER="$1"
fi

# update git index
git status > /dev/null

if ! git diff-index --quiet HEAD; then
	echo -e "\033[33mWARNING: Dirty working tree.\033[0m"
fi

if [[ "$(git rev-parse --abbrev-ref HEAD)" != "master" ]]; then
	echo -e "\033[31mERROR: Not on branch master\033[0m"
	exit 1
fi

# ToolsForCategoricalTowers
git subtree split --prefix=ToolsForCategoricalTowers -b ToolsForCategoricalTowers-split
git push git@github.com:${GITHUB_USER}/ToolsForCategoricalTowers.jl.git ToolsForCategoricalTowers-split:master
echo "Pushed to ToolsForCategoricalTowers.jl"

# QuotientCategories
git subtree split --prefix=QuotientCategories -b QuotientCategories-split
git push git@github.com:${GITHUB_USER}/QuotientCategories.jl.git QuotientCategories-split:master
echo "Pushed to QuotientCategories.jl"

# FpCategories
git subtree split --prefix=FpCategories -b FpCategories-split
git push git@github.com:${GITHUB_USER}/FpCategories.jl.git FpCategories-split:master
echo "Pushed to FpCategories.jl"
