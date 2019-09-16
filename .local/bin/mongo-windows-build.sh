mkdir -p src/mongo-tools
touch src/mongo-tools/{bsondump,mongodump,mongoexport,mongofiles,mongoimport,mongoreplay,mongorestore,mongostat,mongotop}.exe
python ./buildscripts/scons.py dist msi --use-new-tools
