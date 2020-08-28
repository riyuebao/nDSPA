**Scaling & Normalization**

Data Scaling and Normalization are applied in real time on given data and changes will be visible under "Data Plots". The default recommended method for most method is to apply normalization by geometric mean without scaling.

*Scaling Options*

 - Surface Area - Available with all DSP runs, this will scale by the AOI surface area
 - Nuclei Count - Some datasets may have access to nuclei counts as an estimate of cell count

The default calculation method for scaling is by geometric mean

*Normalization Options*

 - Housekeepers - When comparing similar ROI and segment types, this is the recommended method of normalization to account for variance between regions
 - Background (Signal-to-Noise Ratio) - A ratio of counts of the desired probe and a non-specific control. Useful in determining the level of non-specific binding occurring within a region

The default calculation method for normalization is by geometric mean