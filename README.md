# Econ Thesis - CDMX Road Incidents DiD Matching Neighborhoods

18/07/2024

Hello! 
This is my thesis project for my economics major, in it I try to gauge externality effects the closing and opening of new lines of public transportation had in reported road incidents in CDMX.
While the effects i found were marginal i learnt a lot of cool things in doing this about programing, open data, statistics, and doing research such as:

* The importance of publicly available high quality open data in order to assess the impact of public.
* How to handle .shp objects and do cool maps with them in ggplot as well as in QGIS. Also how to polygons to lines to points in R using the simple features `sf` library.
* How to use the `did` library and extract valuable information from a quasi-experimental design.
* How to match trated observation units to untreated ones to creaty a more balanced control group using the `MatchIt` package

Actually this is a new take on my previous political science thesis for which I also made a repository which can be found [here](https://github.com/daveedortega/did_mobility_cdmx).

The main gist of the project is that from 2018 to 2024 there was a change in Mexico City's strategy regarding how to tackle mobility needs in the city; from investing in concessioned roads to unifying the public mobility system and creating new public transit lines in the outskirts of the city to connect previously marginalized communities. On the flipside the [tragedy](https://en.wikipedia.org/wiki/Mexico_City_Metro_overpass_collapse) regarding the collapse of the subways overpass closed a transit line, furthermore the repairs given to the subway's L1 created another closing situation in which a public transit line was closed. 

This I argue created a setting in which some neighborhoods were treated by an exogenous persisting effect, either the opening or closing of a line which, through a substitution effect must have added or subtracted road traffic and thus increased or decreased the amount of road incidents through said neighborhoods.

![Lines Opening](https://www.capital21.cdmx.gob.mx/noticias/wp-content/uploads/2021/03/inauguran_cablebus_05.jpg)
![Lines Closing](https://www.infobae.com/new-resizer/_ZykVmsQAkQlHMTM_nVY6FZDWZA=/992x614/filters:format(webp):quality(85)/cloudfront-us-east-1.images.arcpublishing.com/infobae/KLVXSJBYWVCBTBYDPN2SUJKUVA.jpg)


I use a Difference in Difference design to calculate the effect of the intervention such that, through the difference in difference ATT estimator i can obtain the effect of this in intervention $(\delta)$. Explitinng this design I hope to capture the time and individual heterogeneities in each neighborhood using a simmilar yet untreated set of neighborhoods and as such get: <br> 
$Y = \alpha_i + \eta_t + \delta + \epsilon$ <br>
$\hat{\delta} = (y^T_{post} - y^T_{pre}) - (y^U_{post} - y^U_{pre})$

Since Mexico City is huge and holds over 2,000 neighborhoods I was worried that the comparison from the treated units to the control would be unfair. As such, to correctly asses the difference I matched the treated units with several covariates such as roads and organized transit crossing through neighborhoods, population, and a marginalization index. 

Anyhow, the results are at most marginal, even if they confirm my innitial intuitions we discovered:
1. The closing of a public transit line created an **increase of 1.2 road incidents IN TOTAL**
2. The opening of a public transit line **decreased road incidents in 1.3 incidents IN TOTAL**

Still, I feel like this is useful to learn how to match neighborhoods to public data and crate an experimental design. 
Everything except the data can be found in the scrip BUT all the data I used BUT all data used was provided by public authorities and as such i include it in the following list:

## List of Data:

1. Road Incidents: https://datos.cdmx.gob.mx/dataset/incidentes-viales-c5
2. Neighborhoods maps: http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/Colonia/imc2020_shp.zip
3. Metrobús Lines maps: https://datos.cdmx.gob.mx/dataset/geolocalizacion-metrobus
4. Subway (Metro) Lines: https://datos.cdmx.gob.mx/dataset/lineas-y-estaciones-del-metro
5. Cablebús L1: https://www.google.com/maps/d/viewer?mid=1oMdN93ldNzWajJd71Posa3kyrriFYfqM&femb=1&ll=19.527070428047303%2C-99.132169&z=13
6. Cablebús L2: https://www.google.com/maps/d/viewer?mid=1K71K1Ies8kGq5Gq1AhkBhEVHD6wejGuC&femb=1&ll=19.343885217353048%2C-99.02604699999999&z=14
7. Trolebús L9: https://www.ste.cdmx.gob.mx/linea-9](https://www.google.com/maps/d/viewer?mid=1pAQUGOTxXOEI1VhCpgXOAHRRi6z6dS15&femb=1&ll=19.389136040213213%2C-99.09273660000001&z=13
8. Trolebús L10 (Elevado): https://www.google.com/maps/d/viewer?mid=1v0m4sq7XgP-UeJgLlal-W_M_y2cVTCs&femb=1&ll=19.35089294174388%2C-99.02892929999997&z=14
9. Main Roads in Mexico City: https://datos.cdmx.gob.mx/dataset/vialidades-de-la-ciudad-de-mexico

Aaand that's it. 

If you have any doubts on how to reproduce this you can contact me at david.ortega.alfaro@gmail.com
:) - DAOA

