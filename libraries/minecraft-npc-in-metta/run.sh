mvn clean package
mvn dependency:build-classpath -Dmdep.outputFile=classpath.txt
LD_LIBRARY_PATH=/usr/local/lib/swipl/lib/x86_64-linux/
java -cp "$(cat classpath.txt):target/mettalog.minecraft-1.0-SNAPSHOT.jar" io.trueagi.mettalog.minecraft.BotController

