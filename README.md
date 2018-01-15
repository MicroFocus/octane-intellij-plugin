# octane-intellij-plugin
## IntelliJ IDEA IDE Integration for ALM Octane

The plugin shares a common service layer with: https://github.com/MicroFocus/octane-eclipse-plugin <br>
Common: https://github.com/MicroFocus/octane-plugin-common <br>

Connection to the server is done using the REST SDK for ALM Octane, see: <br>
* https://github.com/MicroFocus/ALMOctaneJavaRESTSDK

### How to build:
The plugin is build using gradle, with the https://github.com/JetBrains/gradle-intellij-plugin

Go to the project main folder and run: 
```
gradle buildPlugin
```

### How to debug: 
```
gradle runIdea
```
You can run this gradle task from IntelliJ in debug mode.