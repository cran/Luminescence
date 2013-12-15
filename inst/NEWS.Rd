\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.3.1 (15th December, 2013)}{
       
     \subsection{S4-classes and methods}{
       \itemize{
    
      \item Added S4-class \code{RLum.Data.Spectrum} for TL/OSL spectra. 
	    \item \code{set_RLum.Data.Spectrum()}: Constructor method for 
             \code{RLum.Data.Spectrum} objects
      \item \code{merge_RLum.Results()}: Correct an error that results in a false
            combination of list elements   
		 \item \code{get_Risoe.BINfileData()}: get-method has been added formally. 
            In fact it does not allow a direct access, but is showing 
            a message.        

		 \item \code{get_RLum.Analysis()}: Added argument \code{curveType} to method. 
     \item \code{get_RLum.Analysis()}: Allow more flexibility using the argument 
     \code{recordType} to method. 
     \item \code{get_RLum.Analysis()}: Added argument \code{RLum.type} to method.
     \item \code{RLum.Results()}: Add generic show method (#68)
     \item \code{RLum.Data.Curve()}: get-method for object class has been added (#57)
       }}
       
       
    \subsection{New functions}{
       \itemize{

     \item \code{plot_RLum.Data.Spectrum()}: Plot function for corresponding 
     S4-class object
     
     \item \code{length_RLum.Analyis()}: Return length (i.e. number of records) for
     \code{RLum.Analysis} objects
     
     \item \code{calc_MaxDose3()}: Apply the maximum age model to a given De distribution 
     (#45, suggestion by Kathryn Fitzsimmons)
    
    
    }}

	   \subsection{Bugfixes and changes}{
       \itemize{
       
       \item \code{calc_MinDose3()}: Graphical output has been changed from 
       postscript to display device output; \code{output.plot = TRUE} as default
       \item \code{calc_MinDose3()}: Implement \code{RLum.Results} object 
       handling (input/output)
		 \item \code{calc_MinDose4()}: Graphical output has been changed from 
     postscript to display device output; \code{output.plot = TRUE} as default 
      \item \code{calc_MinDose4()}: Implement \code{RLum.Results} object 
      handling (input/output)
      \item \code{calc_CommonDose()}: Implement \code{RLum.Results} object handling (input/output)
      \item \code{calc_FiniteMixture()}: Implement \code{RLum.Results} object handling (input/output)
      \item \code{calc_HomogenityTest()}: Implement \code{RLum.Results} object handling (input/output)
        \item \code{calc_CentralDose()}: Implement \code{RLum.Results} object handling (input/output)
        \item \code{calc_CentralDose()}: Now working with data sets with no overdispersion (#65)
		 \item \code{plot_KDE()}: Option \code{na.exclude} did not work as expected and ended up with an error message; corrected
     \item \code{plot_KDE()}: Add statistic paramter for kurtosis and skewness
     for the argument \code{summary}
     \item \code{plot_KDE()}: Add argument \code{output}
     \item \code{plot_KDE()}: Add output values \code{summary.pos}
       \item \code{plot_KDE()}: Now allows keywords for \code{summary.pos}, e.g. 'topleft'
		 \item \code{plot_RLum.Analysis()}: \code{log} argument will now be passed to the output plot
    \item \code{plot_GrowthCurve()}: Now an additional \code{Formula} object containing the equation used
    for fitting is provided with the output.
    \item \code{analyse_SAR.CWOSL()}: Argument \code{output.plot.single} added to allow for separate plot outputs
    \item \code{plot_RadialPlot}: Function has been completely new written
	 \item \code{readXSYG2R}: Add support for TL/OSL spectra has been added by using \code{RLum.Data.Spectrum} objects
   \item \code{plot_RLum}: Add support for \code{RLum.Data.Spectrum}
	\item \code{plot_RLum.Analysis}: Add support for \code{RLum.Data.Spectrum}
  \item \code{plot_RLum.Data.Curve}: Forward further plot arguments, like \code{lwd}. 
  \code{pch}, \code{lty}
  \item \code{readXSYG2R}: Considerably import performance improvements by using the argument \code{useInternalNodes = TRUE} during XML parsing 
  \item \code{readXSYG2R}: Add argument \code{recalculate.TL.curves} to allow temperature 
  values instead of time values in the x-axis for TL curves; along with this option 
  the heating rate will be calculated and stored in the info element \code{RATE} 
  \item \code{plot_RLum.Data.Curve}: Allow for more flexible \code{TL} curve detection
  \item \code{calc_AliquotSize()}: Fix return values (#66)
  \item \code{calc_CosmicDoseRate()}: Fitting procedure for the soft-component optimised (#75)
\item \code{calc_CosmicDoseRate()}: Erroneous estimation of parameters F, J and H fixed. Non-linear parts now fitted with a 3-degree polynomial (#76)
\item \code{calc_CosmicDoseRate()}: Conversion from geographic to geomagnetic latitude fixed (#71)
  \item \code{fit_LMCurve()}: Correct for an error in the plotting of the component
  to sum contribution
   \item \code{fit_CWCurve()}: Correct for an error in the plotting of the component
  to sum contribution
     \item \code{fit_CWCurve()}: Improve convergence stability during fitting by using 
     a two step fitting approach; add comment that the functions has beta status
          \item \code{fit_CWCurve()}: Correct for an error, for component signal 
          plotting (#77)
          \item \code{calc_OSLLxTxRatio}: Add error message for the case signal integral > background integral
  
    
       }}

		\subsection{Example data}{
		\itemize{

		\item \code{ExampleData.RLum.Analysis}: data set has been replaced, as the containing curve elements did not fit the changed \code{RLum.Data.Curve} object
		
		}	
		}

       \subsection{Internals}{
       \itemize{
       
       \item Complete conversion to \code{inlinedoc} documentation for the entire 
       package
       
       \item Allow more fun during plotting. 
       
       }}


}

\section{Changes in version 0.3 (1st October, 2013)}{
 
  \subsection{Substantial Changes}{
  	
  	\itemize{ 
     \item The most substantial changes in this package version are introduced by 
     the usage of S4 objects. Inherited classes have been designed to store input
     and output data for luminescence dating data analysis. Most of the existing 
     functions now allow RLum objects as input data along with generic 
     R objects. The S4-class structure in this package is shown in the additional 
     PDF provided with this package. The following S4 object classes are implemented:
     }}
     
  
    \subsection{Added S4-classes}{
	\itemize{
     
  	  \item \code{RLum} (virtual class)
      \item \code{RLum.Data} (virtual class)
      \item \code{RLum.Data.Curve} (object containing one measured curve)
      \item \code{RLum.Analysis} (contains RLum.Data (e.g. RLum.Data.Curve) objects for protocol analysis)
      \item \code{RLum.Results} (contains output of package functions)
  	  }}
  
     \subsection{Added S4-class methods}{ 
     \itemize{
      \item \code{set_RLum.Data.Curve()}: Constructor method for \code{RLum.Data.Curve} objects
	    \item \code{set_RLum.Analysis()}: Constructor method for \code{RLum.Analysis} objects
	    \item \code{get_RLum.Analyis()}: Accessor method for \code{RLum.Analysis} objects
	    \item \code{set_RLum.Results()}: Constructor method for \code{RLum.Results} objects
    	\item \code{get_RLum.Results()}: Accessor method for \code{RLum.Results} objects
      \item \code{merge_RLum.Results()}: Function to merge \code{RLum.Results} objects
	    \item \code{get_structure.RLum.Analysis()}: to parse the structure of an \code{RLum.Analysis} object
	    \item \code{set_Risoe.BINfileData()}: Constructor method for \code{Risoe.BINfileData} objects
    }}
    
    \subsection{New Functions}{
    \itemize{
     \item \code{plot_RLum()}: Wrapper function for plotting various \code{RLum}-objects
     \item \code{plot_RLum.Data.Curve()}: Plot function for \code{RLum.Data.Curve} objects. Usually accessed by \code{plot_RLum()}
     \item \code{plot_RLum.Analysis()}: Plot function for \code{RLum.Analysis} objects. Usually accessed by \code{plot_RLum()}
	 \item \code{Risoe.BINfileData2RLum.Analysis()}: Converts parts of an \code{Risoe.BINfileData} object to an \code{RLum.Analysis} object, depending on the chosen position, run/set or record id.
     \item \code{Risoe.BINfileData2RLum.Data.Curve()}: Lossy conversion of an \code{Risoe.BINfileData} object to an \code{RLum.Data.Curve} object
	 \item \code{writeR2BIN()}: Function to export \code{Risoe.BINfileData} objects to BIN or BINX files
	 \item \code{merge_Risoe.BINfileData()}: Merge BIN/BINX files or \code{Risoe.BINfileData} objects
	 \item \code{analyse_SAR.CWOSL()}: Function to analyse SAR CW-OSL measurements, will at some time replace the exisiting function \code{Analyse_SAR.OSLdata}
	 \item \code{analyse_SAR.TL()}: Function to analyse SAR TL measurements (beta version)
	 \item \code{calc_TLLxTxRatio()}: Function to calculate Lx/Tx ratios for TL curves used by \code{analyse_SAR.TL()} (beta version)
	 \item \code{analyse_IRSAR.RF()}: Function to analyse IRSAR RF measurements according to Erfurt et al. (2003)
   \item \code{calc_CosmicDoseRate()}: Function to calculate the cosmic dose rate
   \item \code{calc_HomogeneityTest()}: Function to apply the homogenity test after Galbraith (2003)
   \item \code{calc_AliquotSize()}: Function to estimate the amount of grains on an aliquot
   \item \code{readXSYG2R()}: Function to support the Freiberg Instruments XSYG file format
  
    }}

     \subsection{New Data Sets}{
    \itemize{
      \item \code{ExampleData.RLum.Analysis}: Luminescence dating measurement dataset as \code{RLum.Analysis} object
     	\item \code{BaseDataSet.CosmicDoseRate}: Data set used by the function \code{calc_CosmicDoseRate()}
      \item \code{ExampleData.BINfileData}: dataset now contains two objects \code{CWOSL.SAR.Data} and \code{TL.SAR.Data} (depending examples have been modified)
 
    }}
      
    \subsection{Function Renaming}{
    \itemize{
     \item \code{Calc_OSLLxTxRatio()}: Function renamed to \code{calc_OSLLxTxRatio()}
     \item \code{Calc_CommonDose()}: Function renamed to \code{calc_CommonDose()}
     \item \code{Calc_CentralDose()}: Function renamed to \code{calc_CentralDose()}
	   \item \code{Calc_FiniteMixture()}: Function renamed to \code{calc_FiniteMixture()}
	   \item \code{Calc_MinDose3()}: Function renamed to \code{calc_MinDose3()}
	   \item \code{Calc_MinDose4()}: Function renamed to \code{calc_MinDose4()}
	   \item \code{Calc_FadingCorr()}: Function renamed to \code{calc_FadingCorr()}
	   \item \code{Calc_FuchsLang2001()}: Function renamed to \code{calc_FuchsLang2001()}
     \item \code{plot_BINfileData()}: Function renamed to \code{plot_Risoe.BINfileData()} for the sake of consistency 
  
    }}
      
    \subsection{Bug Fixes and Further Changes}{
      \itemize{
    
     \item \code{calc_OSLLxTxRatio()}: Function now allows for a \code{matrix} as input object 
     \item \code{calc_OSLLxTxRatio()}: Function now returns an \code{RLum.Results} object
	   \item \code{Analyse_SAR.OSLdata()}: Function modified according to the changes in the function \code{calc_OSLLxTxRatio()}
	    \item \code{Analyse_SAR.OSLdata()}: Function corrected for an error that occured with more than one recyling point
	    \item \code{Analyse_SAR.OSLdata()}: Function corrected for an error that occured when the dose of the first recycling point was used twice
     \item \code{Risoe.BINfileData-class}: An error in the show method has been corrected 
     \item \code{plot_GrowthCurve()}: An error occurred if no fitting could be applied. Corrected by setting De to NA
	   \item \code{plot_GrowthCurve()}: The function now checks if at least two regeneration points are given
     \item \code{plot_GrowthCurve()}: Function corrected for an infinite loop error occured using only two dose points and the EXP fit option
     \item \code{plot_GrowthCurve()}: The function now provides an extra argument \code{na.exclude} to exclude \code{NA} values
     \item \code{plot_KDE()}: The function now provides an extra argument \code{na.exclude} to exclude \code{NA} values
     \item \code{plot_KDE()}: The function now allows for an \code{RLum.Results} object as input object
     \item \code{plot_KDE()}: The function now allows for plotting multiple data sets
     \item \code{plot_Histogram()}: The function now provides an extra argument \code{na.exclude} to exclude \code{NA} values
     \item \code{plot_Histogram()}: The function now allows for an \code{RLum.Results} object as input object
     \item \code{plot_RadialPlot()}: The function now provides an extra argument \code{na.exclude} to exclude \code{NA} values
	   \item \code{CW2pLMi()}: A test has been implemented to check if  P=0 (which is not allowed)
	   \item \code{CW2pLMi()}: Now an \code{RLum.Data.Curve} object is allowed as input and an object of the same data type as the input will be returned
	  \item \code{CW2pHMi()}: Now an \code{RLum.Data.Curve} object is allowed as input and an object of the same data type as the input will be returned
	   \item \code{CW2pPMi()}: Now an \code{RLum.Data.Curve} object is allowed as input and an object of the same data type as the input will be returned
	   \item \code{CW2pLM()}: Now an \code{RLum.Data.Curve} object is allowed as input and an object of the same data type as the input will be returned
	   \item \code{fit_CWCurve()}: The function now returns an \code{RLum.Results} object and as input an \code{RLum.Data.Curve} object is now allowed
       \item \code{fit_CWCure()}: An additional \code{component.contribution.matrix} is now returned to allow 
     for individual component to sum contribution plots 
     \item \code{fit_CWCure()}: A critical error has been corrected in the used fitting equation. Now it has been set to f(t) = I0[i]*lambda[i]*exp(-lambda[i]*t)
       \item \code{fit_LMCurve()}: The function now returns an \code{RLum.Results} object and as input an \code{RLum.Data.Curve} object is now allowed
     \item \code{fit_LMCure()}: An additional \code{component.contribution.matrix} is now returned to allow 
     for individual component to sum contribution plots 
     \item \code{readBIN2R()}: BINX file format support has been added 
     \item \code{readBIN2R()}: An argument \code{forced.VersionNumber} has been added to cheat the internal version check routine if needed
     \item \code{Risoe.BINfileData-class}: The *.binx file format support has been added 
	  \item \code{RLum.Analysis}: Add argument \code{get.index} just to show the index values
  
    }}

    \subsection{Internals}{
    \itemize{
     \item We partially started to create package documentation using the R pacakge 'inlinedocs', meaning that 
     the documentation is now be part of the source code itself
     \item A namespace .LuminescenceEnv in zzz.R has been added 
     \item A variable \code{col} has been added to the namespace \code{.LuminescenceEnv} in the file zzz.R to define a standard colour palette for graphical output in package
     \item NEWS are now provided as *.Rd file; der corresponding ASCII file is produced using the function Rd2txt
     \item An unicode character for the temperature degree symbol 'deg. C' has been added
     \item The S4-object structure has been added as PDF documentation to the package
    
    }}

  }
