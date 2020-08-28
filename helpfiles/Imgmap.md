**Image Map**

Currently the Image Map requires the user to take a screenshot of the slide from the GeoMX software. This can be achieved using the "Snip & Sketch" tool in Windows 10 or by taking a screenshot and saving a cropped '.png' file in your favorite image editor. Currently only PNG is is a verified format for this tool.

Please follow these steps:

 1. Load your count data into nDSPA
 2. Apply QC, Scaling and Normalization
 3. Use the classification data importer to specify a classification file
 4. Under "Expression Map" click the gear icon and load the image of your slide
 5. Select the appropriate scan name in the Expression Map Selector
 6. Select the probe you wish to view. This data will be plotted over your provided image.

X and Y coordinates for the data points can be approximated in many image manipulation application which provide you with cursor position relative the the image. "Paint" included with windows provides this data in the lower right information box. All points should be relative to standard image coordinates, where the top left corner is the origin (0,0) and data must be provided in pixels.

 - Example Classification Table

| ROI |Segment  | x | y |
|--|--|--|--|
| 001 | Seg1 | 300| 400|
|001|Seg2|300|400|
| 002 |Seg1  |604 |37 |
|003|Seg1|50|103|
| 003 | Seg2 |50 |103 |


