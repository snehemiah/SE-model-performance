# SE-model-performance

# Background
This project aimed to evaluate the performance of spatially-explicit statistical catch-at-age models (descriped in Nehemiah et al. in review) compared to spatially-implicit (fleets-as-areas) approaches. Using Atlantic Striped Bass, we developed a simulation study to assess accuracy of spatially-explicit multi-stock and spatially-implicit single stock assessment models with alternative scenarios of data availability and data quality. Specifically we evlauted the effect of ageing error and stock composition data on model accuracy.

 All models were developed in ADMB (https://www.admb-project.org/). 

# Model Descriptions
1. **FAA1** - This model is a spatially-implicit, fleets-as-areas model with two fleets that resembled the current assessment model used to inform management. The two fleets for were the Chesapeake Bay fleet and the Ocean fleet. This model assumed a single region, a single unit stock, and annual time-steps, and did not correct for ageing error
2. **FAA2** - Similar to FAA1, except it assumes ageing error in the age composition and applies and ageing error matrix. 
3. **SE1** - This model is a two spawning stock (Atlantic Coast and Chesapeake Bay), two area (Ocean and Chesapeake Bay) model that estimates abundance, movement, fishing mortality, and biomass at two 6-month time-steps for 30 years. This model does not assume error in the age composition data.  
4. **SE2** - Similar to SE1 except it assumes ageing error in the age composition and applies an ageing error matrix.  
5. **SE3** - SE model that incorporated additional data on stock composition for the last ten years and included ageing error, but it did not include informative priors on the occupancy probabilities

For a complete description of SE1 and SE2, see: Nehemiah et al. (in review)

# Code Available
R Code is availablefor the full simulation. Estimation models were coded in ADMB. 

# Partner Institutions
This work was funded by the NOAA Chesapeake Bay Office and the NMFS/Sea Grant Population and Ecosystems Dynamic Fellowship. Collaborators include researchers from the Chesapeake Biological Laboratory and the NMFS Southeast Fisheries Scienct Center.

# Manuscript Citation
Nehemiah, S., A.M. Schueller, and M.J. Wilberg. Effects of model misspecification and data availability on spatially-explicit assessment model performance. In Prep.

# Contact
If you have any questions, please contact Samara (she/her) at snehemiah@asmfc.org. 

<img src="https://www.umces.edu/sites/default/files/UMCES-CBL-logo.jpg" jsaction="" class="sFlh5c pT0Scc iPVvYb" style="max-width: 600px; height: 221px; margin: 0px; width: 557px;" alt="UMCES CBL logo.jpg | University of Maryland Center for Environmental Science" jsname="kn3ccd" aria-hidden="false">
