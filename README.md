# octane-intellij-plugin
For developers, by developers! This plugin is built with gradle and uses the latest public SDK of ALM Octane

###How to build:
We are building the octane plugin with gradle, using the https://github.com/JetBrains/gradle-intellij-plugin

Go to the project main folder and run: 
```
gradle buildPlugin
```

### How to debug: 
```
gradle runIdea
```
If you want to use breakpoints I recommend importing the code into Intellij and running the runIdea gradle task with debug.
