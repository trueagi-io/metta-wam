mvn clean package
mvn dependency:build-classpath -Dmdep.outputFile=classpath.txt
LD_LIBRARY_PATH=/usr/local/lib/swipl/lib/x86_64-linux/
export CLASSPATH="$(cat classpath.txt):$(pwd)/target/mettalog.minecraft-1.0-SNAPSHOT.jar"
java -cp "$CLASSPATH" io.trueagi.mettalog.minecraft.BotController

