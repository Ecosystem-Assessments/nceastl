#' Pipeline to execute full cumulative effects assessment.
#'
#' @export

pipeline <- function() {
  # Prepare data for the assessment
  format_data()
  
  # Assessment 
  # ----------------------------------------------------------------------------------------
  # NOTE: The species-scale assessment is lightweight and should not need to run on a cluster 
  #       In case it does, use the 'R/.cea.R' script and build a 'run.sh' script to use it
  # NOTE: The network-scale assessment must be run on compute canada servers
  #       The formated data and scripts must be loaded on compute canada
  #       R and dependencies must also be installed 
  #       Once setup is complete, run sbatch script.sh
  #       `sbatch .ncea.sh` for .ncea.R, or 'source("R/.ncea.R")'
  #
  # NOTE: .sh files should look something like this for non-array computation (no iterations) 
  # 
  # #!/bin/bash
  # #SBATCH --account=name of acount
  # #SBATCH --mem=125GB
  # #SBATCH --time=5:00:00
  # #SBATCH --mail-user=youremail@provider.com
  # #SBATCH --mail-type=ALL
  # 
  # module load StdEnv/2020 r/4.2.1 gcc/9.3.0 geos/3.10.2 gdal/3.5.1 proj/9.0.1 udunits
  # Rscript ./.cea.R
  #
  # NOTE: .sh files should look something like this for array computation (with iterations) 
  #
  # #!/bin/bash
  # #SBATCH --account=name of acount
  # #SBATCH --mem=125GB
  # #SBATCH --time=10:00:00
  # #SBATCH --array=1-193
  # #SBATCH --mail-user=youremail@provider.com
  # #SBATCH --mail-type=ALL
  # 
  # module load StdEnv/2020 r/4.2.1 gcc/9.3.0 geos/3.10.2 gdal/3.5.1 proj/9.0.1 udunits
  # Rscript ./ncea_species.R $SLURM_ARRAY_TASK_ID
  # ----------------------------------------------------------------------------------------
  cea()
  # source("R/.ncea_init.R")
  # source("R/.ncea.R")  
  
  # Extract results needed for manuscript from assessment
  out_cea_full()
  out_cea_km2()
  out_cea_region()
  out_species_contribution()
  out_anomalies()
  out_mechanisms()
  
  # Main figures
  fig_method2() # Figure 1 
  fig_ncea_full() # Figure 2
  fig_comparison() # Figure 3
  fig_contribution() # Figure 4
  
  # Extended figures & tables (maximum 10 for Nature)
  # fig_stressors() # Cite Frontiers in Marine Science paper instead
  fig_comparison_spatial() 
  # fig_groups_spatial()
  fig_species_spatial()
  fig_contribution_taxa()
  fig_species_contribution()
  # tab_stressors()
  # tab_biotic()
  # tab_abiotic()
  # tab_traits()
  # tab_species_sensitivity()
  # tab_r_packages()
  
  # Supplementary information 
  # tab_species()
  
  ## Publications 
  render_ms(TRUE)
  render_ms_supmat(TRUE)
}