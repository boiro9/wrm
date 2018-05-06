We recommend the installation of the **wrm** package from the hubgit repository. So, first of all, install de **devtools** R package:

```
install.packages ("devtools")
```

Once installed, install the following packages typing in your R console:

```
devtools::install_github ('jorgerodriguezveiga/romo')
devtools::install_github ('jorgerodriguezveiga/WildfireResources')
devtools::install_github ('mjginzo/SAA')
```

Once the installation is complete, you should be able to execute the command line that the interface loads:

```
wrm::shinyapp ()
```

To check the installation download the **example** folder and from the opened interfaz, try to load the example stored in the feasible subfolder.
