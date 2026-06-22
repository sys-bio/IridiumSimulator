# Iridium

 <table style="width:100%">
  <tr>
    <td><img alt="Licence", src="https://img.shields.io/badge/License-Apache%202.0-yellowgreen"></td>
    <td><img alt="Funding", src="https://img.shields.io/badge/Funding-NIH%20(GM123032)-blue"></td>
   <td><img alt="Language", src="https://img.shields.io/badge/Delphi-11-blue.svg"></td>
   <td><img alt="GitHub all releases" src="https://img.shields.io/github/downloads/sys-bio/IridiumSimulator/total?color=red&style=plastic"> </td>
   </tr>
</table> 

   
### This is a new rewrite of the original Iridium Simulator PLatoform ###
 
 Interactive desktop simulator (Windows/Mac) for systems biology. Built on top of the simulation engine [libroadrunner](https://github.com/sys-bio/roadrunner).

The current version suppotrs: Time course simulation, steady-state, real-time interactive simulation via sliders, parameter scans, and MCA style sensitivity analysis (https://en.wikipedia.org/wiki/Metabolic_control_analysis). It uses the [antimony](https://github.com/sys-bio/antimony) format to describe models and is fully [SBML](https://github.com/sbmlteam/libsbml) compliant.

I would like to thank Guillermo Canedo Ramirez who did an excellent job in making the 3D bar plotting compoment (https://github.com/gcanedo/T3DBarGraph)

The web version which runs inside the browser can be found at

Source code: https://github.com/sys-bio/WebIridium

GitHub page: https://sys-bio.github.io/WebIridium/

Here are some screenshots to show you what it looks like:

Time course simulation:

<img src="/Images/iridium1.png" width="80%"></img> 

Time course parameter scan:

<img src="/Images/iridium2.png" width="80%"></img> 

Control coefficients 3D plot (random network):

<img src="/Images/iridium3.png" width="80%"></img> 

Control coefficients 3D plot (random network):

<img src="/Images/iridium4.png" width="80%"></img> 

Millard E coli model (2016)

Control coefficient matrix:

<img src="/Images/iridium5.png" width="80%"></img> 
