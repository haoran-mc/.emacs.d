#!/bin/bash

# 使用第一个参数作为目录，如果没传就用当前目录
DIR=${1:-.}

# 避免路径里有空格时出错
find "$DIR" -name "*.md" -type f | while read -r md; do
  echo "$md"
  pandoc -f markdown -t org -o "${md%.*}.org" "$md"
done

