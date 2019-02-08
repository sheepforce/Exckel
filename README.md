# Exckel
Exckel generates documents to summarise and visualise the results of excited state calculations in quantum chemistry. It automates the following task

  - parse the output file of the quantum chemistry software
  - select/filter excited states by their $<S^2>$ and/or their minimum oscillator strength
  - plot the spectrum (with conversion to absorption coefficient and wavelength)
  - calculate orbital and charge difference density cubes
  - render images of the cubes
  - write a summary document, which tabulates excitations and images of orbitals involved and the CDDs of the excited states

Exckel is completely written in Haskell, but basically it glues together existing solutions for the individual tasks.

- *Quantum chemistry software supported:*
  - [Gaussian](gaussian.com) TDDFT/CIS
- *Cube calculator:*
  - [Multiwfn](http://sobereva.com/multiwfn/) (tested with 3.5, gMultiwfn 3.4.1)
- *Cube renderer:*
  - [VMD](https://www.ks.uiuc.edu/Research/vmd/) (tested with 1.9.3) & [Tachyon](http://jedi.ks.uiuc.edu/~johns/raytracer/)


## Usage
Exckel is a command line programm and can be easily called from a bash script or simply directly from the command line. The minimum input you will need is the output file from your quantum chemistry code and a wavefunction file (molden or fchk).

By calling `exckel --help`, a quick explanation of all possible keywords is provided.

exckelargs [OPTIONS]
Available command line arguments. At least "--wf" and "--exc" must be
specified.

      Common flags:
       --nocalccubes         Do not calculate cubes for orbitals and CDDs.
       --norenderimages      Do not render images from cubes.
    -o --outdir=DIR          Destination for all output files and existing
                             cubes.
       --vmd=FILE            VMD executable. Default is first vmd executable
                             found on system.
       --vmdstate=FILE       VMD visualisation state file. Used to determine
                             perspective.
       --vmdstartup=FILE     VMD script to set up general look. If none is
                             specified, it will default to your vmdrc.
       --vmdtemplate=FILE    VMD template script for plotting.
    -m --multiwfn=FILE       Multiwfn executable. Default is first Multiwfn
                             executable found on system
    -t --tachyon=FILE        Tachyon executable. Default is first tachyon
                             executable found on system
       --pdformat=STRING     Format of the summary to write with Pandoc. Any of
                             [docx, odt, latex]
       --panref=FILE         Reference docx with formatting hints.
       --pandir=DIR          Pandoc data dir. Needed for serveral formats.
       --wf=FILE             Wavefunction file (molden or fchk).
    -e --exc=FILE            Quantum chemistry software output file with
                             excited state informations.
    -i --imgres=INT,INT      Image width x heigth for plotting of cubes.
    -s --s2filter=FLOAT      Filter excited states by contributions of next
                             higher spin state (applies to plotting and summary).
       --foscfilter=FLOAT    Filter excited states by minimum oscillator
                             strength (applies only to summary document).
       --fwhm=FLOAT          Full width at half maximum of the gaussian
                             function used to convolute the stick spectrum.
       --weightfilter=FLOAT  Minimum weight of an excitation to write to the
                             summary. (default 0.01)
    -? --help                Display help message
    -V --version             Print version information

At least `--wf` and `--exc` must be set and point to your wavefunction file respective your quantum chemistry output file (with excited state information).

The destination of the output (and intermediate files) is determined by `--outdir`, which points to the path, where output shall be written. Next to writing all files to this directory, Exckel will look for potentially already existing files (such as images and cubes) in this directory. If not set, it defaults to the current directory.

An explanation of the workflow and the effects of the parameters follows.

### Parsing, Plotting and Filtering
*Relevant arguments:*
  - `--s2filter`
  - `--foscfilter`
  - `--weightfilter`
  - `--fwhm`

Exckel starts by parsing the quantum chemistry log file and looks for the informations regarding excitations, multiplicity, number of basis functions and so on.

From the $<S^2>$ value of an excited state, one can calculate the contribution of the next higher spin state as a fractional amount. The excited states can be filtered by their spin state purity. As an example lets assume the multiplicity of your reference wavefunction was a triplet ($S = 1$). The  $<S^2>$ value of a pure spin state should be $<S^2> = S * (S + 1) = 2$. From the true $<S^2>$ value of the excited state, the contributions of the quintetts to the state can be calculated. If the fractional contribution of the quintett is higher than the value specified with `--s2filter`, the state will be discarded completely and not used to plot the spectrum, nor will it appear in the summary table, nor will cubes and images for it be calculated.

Excited states can also be filtered by a minimum oscillator strength with `--foscfilter`. States discarded by this filter will still contribute to the spectrum, but will not appear in the summary table and CDDs and orbitals will not be calculate for this state.

If you use a very verbose output (small coefficients printed), you can restrict the number of CI determinants printed in the summary with `--weightfilter`. This will influence the summary table and the orbital cubes, that need to be calculated.

Gnuplot is then used to plot the spectrum (remaining states after `--s2filter`) and save it to `Spectrum.png` in the output directory. A convolution of the stick spectrum is done by gaussian functions, for which the full width at half maximum can be specified with `--fwhm` (in electron Volt).

### Calculating Cubes
*Relevant arguments:*
  - `--outdir`
  - `--multiwfn`
  - `--nocalccubes`

Charge difference densities and and orbitals are stored in cube files. If they are already present, you can use them. You will need to make sure, that they are available in the output directoy and named properly. Orbital cubes must be called `orb${NORB}.cube`, where `$NORB` is the number of the orbital (start counting from 1) and has no leading zeros. Alpha and beta orbitals of unrestricted wavefunctions are distinguished solely by this number, where the alpha orbitals have numbers from 1 to (number of basis functions) and the beta orbitals have numbers from (number of basis functions + 1) to (number of basis functions * 2). Charge difference density cube need to be labeled as `CDD${NSTATE}.cube`, the corresponding holes as `hole${NSTATE}.cube` and the electrons as `electron${NSTATE}.cube`. If cubes exist (from a previous calculation or calculated on a fast computer or something) and you want to use them, specify `--nocalccubes`. _Otherwise existing cubes will be overwritten_.

If cubes are not available yet, Exckel will call an external program to calculate them from the log file and the wavefunction. Be aware, that the precision here is limited by the accuracy of the log file.

Currently only Multiwfn is supported to calculate these cubes. By default the first `Multiwfn` executable found, will be used, but this can be changed by pointing `--multiwfn` to an `Multiwfn` executable.

### Plotting cubes
*Relevant arguments:*
  - `--outdir`
  - `--norenderimages`
  - `--vmd`
  - `--vmdstate`
  - `--vmdstartup`
  - `--vmdtemplate`
  - `--tachyon`
  - `--imgres`

All cubes found in the output directory, which follow  the correct naming scheme, will be plotted, if `--norenderimages` is not specified.

The only visualisation programm currently supported is the combination of VMD with Tachyon. To customise the appearance of the images, a variety of options is available. Most intreresting one is propably the file specified by `--vmdstartup`. This is a VMD Tcl script, which is executed by VMD before doing anything else. Here you can make general settings like colours, camera settings, ... . If this is not specified, Exckel will try going for your `.vmdrc` in the home directory and use this one. Therefore, if your happy with your defaults when opening VMD, you don't need to specify `--vmdstartup`.

The second option to customise the appearance of your images is `--vmdstate`. This argument should point to a VMD visualisation state file. Nothing except the camera perspective is taken from this file, but if it is not specified, you will need to live with the default perspective.

Last, a VMD Tcl template script is specified, which is the working horse here. This specifies everything that is necessary to get actual input files for Tachyon for every cube. The script used by Exckel is available in `data/VMD.tcl` of this repository and uses the ginger template engine to replace `{{ orbs }}` by a list of file base names of orbital cubes and `{{ cdddens }}` by a list of file base names of CDDs, electrons and holes. If you want to use your own Tcl script provide it via `--vmdtemplate`. Make sure it quits VMD properly, otherwise it will stay open forever.

VMD is called, writes input files for Tachyon (`*.dat`) and Tachyon (executable can be specified by `--tachyon`, defaults to first `tachyon` found) is called to render images in the resolution given by `--imgres` (defaults to 2000 x 1200 pixel). Make sure your Tachyon supports png output.

### Writing the Summary Document
*Relevant arguments:*
  - `--outdir`
  - `--pdformat`
  - `--panref`
  - `--pandir`

From everything that is available up to now, the summary document will be created by [Pandoc](http://hackage.haskell.org/package/pandoc). You can choose to get the output as Microsoft Word docx (`--pdformat=docx`), as a LibreOffice/OpenOffice compatible Open Document Text odt (`pdformat=odt`) or as LaTeX source code (`pdformat=latex`).

The exact formatting of the output document depends on reference documents (`--panref`) in the case of ODT and DOCX files. If `--panref` is specified and points to a .docx or .odt document with formatting hints, these will be used. Otherwise a default is provided (and included in the executable, thanks TemplateHaskell) and automatically used.

If you would like to used ODT as the output format, the Pandoc data directory needs to be available and the path to it specified by `--pandir`.

The finaly summary is written to the output directoy as `summary.*`.

### Example
For a Gaussian calculation of benzene, where you have `Benzen.fchk` as the wavefunction and `Benzen.log` as the log file of the calculation and want to write a summary for all excited states with an oscillator strength > 0.1 as a docx.

    exckel --wf=Benzen.fchk --exc=Benzen.log --outdir=exckel-out --foscfilter=0.1 --panformat=docx


## Installation
Exckel needs serveral components to do all its work. Many of them are Haskell libraries and you don't need to take care of them, but calculators for the orbitals, visualisation software and the raytracing engine are system calls to standard quantum chemistry applications. Furthermore ImageMagick and Gnuplot are needed.

If you want to build from source, you will also need Haskell's `stack` tool, which takes care of building.

### Dependencies
Installation of the external programs is up to you. Installation of them is quite easy.
- [Multiwfn](http://sobereva.com/multiwfn/)
- [VMD](https://www.ks.uiuc.edu/Research/vmd/)
  - I highly recommended to customise VMD with the `.vmdrc` to fit your needs. This requires the least complex setup from VMD startup scripts, state files and template scripts.
- [Tachyon](http://jedi.ks.uiuc.edu/~johns/raytracer/)
  - Can be compiled from source, but is also available in many linux distributions. The version bundled with VMD does not support PNG images.
- [Gnuplot](http://www.gnuplot.info/)
  - On Linux is basically always included in the distributions, easy to install on Windows.
- [ImageMagick](https://www.imagemagick.org/)
  - On Linux is basically always included in the distributions, easy to install on Windows.
- If you want to use ODT or LaTeX output [Pandoc](https://github.com/jgm/pandoc)
- If building from source [Haskell's Stack](https://docs.haskellstack.org/en/stable/README/)
  - Can be easily installed as a binary without any knowledge of Haskell

If you need ODT or LaTeX outputs, you will need the Pandoc data directory, which contains necessary templates for these formats. This can be obtained from the git repository of Pandoc:

    git clone https://github.com/jgm/pandoc.git
    cp pandoc-master/data $DEST

Copy the data directory from Pandoc to a persistent destination of your choice and point `--pandir` to it at runtime.

### Building from Source
Clone the repository from git, use stack to install everything.

    git clone https://github.com/sheepforce/Exckel.git
    cd Exckel
    stack install

Likely this will take some time, as especially Pandoc is a very large library, that takes some time to build.

Make sure the installation directory shown by `stack` is in your `PATH`.

### Installing the Binary
For Linux a partially statically linked binary is available, which only requires most fundamental C libraries to be available on your system. Simply copy the binary to a directory of your choice and make sure it is in your `PATH`.
