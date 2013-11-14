DIR=tree-tagger

mkdir $DIR
rm -r $DIR/*
cd $DIR

wget www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/tree-tagger-linux-3.2.tar.gz
wget www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/tagger-scripts.tar.gz
wget www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/install-tagger.sh
wget www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/french-par-linux-3.2.bin.gz

sh install-tagger.sh

cat LICENSE

echo "For more informations about TreeTagger visit www.cis.uni-muenchen.de/~schmid/tools/TreeTagger."
