```{r eval= FALSE}


ggplot() +
  geom_raster( data=results, aes_string(y="Latitude", x="Longitude")) +
  labs(title = title) +
  coord_fixed()

```


```{r}

RasterToggplot <- function(raster, layerName = "Values", factor = TRUE, addNo = TRUE){
  
  RasterValues <- raster::rasterToPoints(raster)
  result_df <- data.frame(RasterValues)
  colnames(result_df) <- c("Longitude", "Latitude", layerName)
  
  # Optional argument if the value is a factor
  if(isTRUE(factor)){
    result_df[[layerName]] <- factor(result_df[[layerName]])
  }
  
  # Optional argument if a No answer value needs to be added
  if(isTRUE(addNo)){
    
    addNoAnswer <- function(x){
      if(is.factor(x)) return(factor(x, levels=c("-99",levels(x))))
      return(x)
    }
    
    result_df[[layerName]] <- addNoAnswer(result_df[[layerName]])
    
    return(result_df) 
  }
  
  
  return(result_df)
}
```


```{r fig.caption = "Results of Generalisation"}
results <- raster::raster("code/PredictedRaster.tif")

raster::plot(results)



```
