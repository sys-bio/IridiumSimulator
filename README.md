# Iridium

 <table style="width:100%">
  <tr>
    <td><img alt="Licence", src="https://img.shields.io/badge/License-Apache%202.0-yellowgreen"></td>
    <td><img alt="Funding", src="https://img.shields.io/badge/Funding-NIH%20(GM123032)-blue"></td>
   <td><img alt="Language", src="https://img.shields.io/badge/Delphi-11-blue.svg"></td>
   </tr>
</table> 

### Coming soon for summer 2022 - Beta has Arrived
 
 
Interactive desktop simulator (Windows/Mac) for systems biology. Built on top of the simulation engine [libroadrunner](https://github.com/sys-bio/roadrunner).

Supports: Time course simulation, steady-state, real-time interactive simulation, parameter scans, slider control, MCA style sensitivity analysis (https://en.wikipedia.org/wiki/Metabolic_control_analysis), and network structural analysis. It uses the [antimony](https://github.com/sys-bio/antimony) format to describe models and is fully [SBML](https://github.com/sbmlteam/libsbml) compliant.

**Update** (Jul 6th): (v0.952) I now have a Mac OS version including the M1 version working (thanks to Lucian Smith for generating the M1 roadrunner binaries for me). I hope to release the source code and binaries for windows and Mac during the week of July 11th.

**Update** (Aug, 11th): Update to v0.954, Windows 64, Apple Intel and Apple M1. Fixed issue with certain models where it wasn't possible to reset back to initial conditions. Added new simualtion option to automatically reset back to iniital conitions or not. Fixed some annoying bugs in the graph plotting (Thanks to Wesely Luk), added a new dark style, accessible from the top-right drop-down list. Enhanced saving of settings, moved the text output panel tp be part of the plotting panel. Moved the basic plotting option to the plotting panel. Source code updated. 

I would like to thank Guillermo Canedo Ramirez who did an excellent job in making the real-time scrolling chart.

Here are some screenshots to show you what it looks like:

<img src="/images/iridium1.png" width="40%"></img> <img src="/images/iridium3.png" width="40%"></img> <img src="/images/iridium2.png" width="40%"></img> 
