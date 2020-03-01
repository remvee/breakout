set -ex

lein package

rm -rf deploy
git clone git@github.com:remvee/breakout.git deploy
cd deploy

git checkout -b gh-pages --track origin/gh-pages

rm -rf -- *
mkdir css js
cp ../public/index.html .
cp ../public/css/site.css css/
cp ../public/js/app.js js/

git add -- *
git commit -m '..'
git push
