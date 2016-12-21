Two kinds of r scripts for this project:
1.  Scrape and Populate the MongoDB, (most of the code)
2.  Small Analysis file (analysisRatingsListens.R) which needs step 1 completed

To populate the Monogodb collections, at the bottom of the "meldRatingsVUse.R" file,
there are 6 commented commands. Uncomment those and run.  EXCEPT, the caveat is that
to use the lastFM.api, you need a key, which much like batteries for physical devices,
is not included.

So as an alternate, run these 6 commands to populate the MongoDB.  

#change the 6 to the depth you want...6 will take a while...2 is good to see how it works
populateArtistCollection(seedURL, "Artists", 6, T)

#this next step takes a proportionally larger amount of time than the step above
populateArtistRepository(getArtistURLs()[,1], "AllMusic")

#PopulateFMDataFromAMTable()  <- not executed
populateFMDataFromFile()  #<-executed instead to load small subset file


dfAllMusic <- getAlbums()
dfLastFM <- getCompleteFMCases()
meshAMFM(dfAllMusic,dfLastFM)

Obviously the analysis won't work as in the presentation as this is a tiny subset of what
was scraped.  Hopefully it will work, I have not tried running this particular subset.


One side note, the Javascript in the markdown to show/hide code was "borrowed" from
a stackoverflow post.
