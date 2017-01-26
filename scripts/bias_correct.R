require(dplyr)
require(qmap)

BiasCorrect <- function(df, method="Quant", wet.day=T) {
    
    if (method == "Li") {
        return(.bc2(df, wet.day))
    }
    
    return(.bc1(df, wet.day))
}

.bc1 <- function(df, wet.day=T) {
    fit_df <- df %>% dplyr::filter(fit)
    
    if (sum(!is.na(fit_df$obs))>10) {
        qm.fit <- fitQmapQUANT(fit_df$obs, fit_df$mod, wet.day)
    }
    
    if (!exists("qm.fit")) {
        out_df <- df %>%
            dplyr::mutate(bc=mod)
    } else {
        out_df <- df %>%
            dplyr::do(data.frame(., bc=doQmap(.$mod, qm.fit)))
    }
    
    return(out_df %>% dplyr::select(-fit))
}

.bc2 <- function(df, wet.day) {
    fit_df <- df %>% dplyr::filter(fit)
	
	out_df <- df %>%
		dplyr::group_by(grp) %>%
		dplyr::do(.bc2_helper(fit_df, ., wet.day)) %>%
		ungroup()
		
	return(out_df %>% dplyr::select(-grp, -fit))
}

.bc2_helper <- function(fit_df, df, wet.day) {
	if (unique(fit_df$grp) %in% unique(df$grp)) {
		return(.bc1(df, wet.day))
	}
	
	if (sum(!is.na(fit_df$obs))>10) {
        qm.fit1 <- fitQmapQUANT(fit_df$obs, df$mod, wet.day)
        qm.fit2 <- fitQmapQUANT(fit_df$mod, df$mod, wet.day)
    }
	
	if (!exists("qm.fit1") | !exists("qm.fit2")) {
        out_df <- df %>%
            dplyr::mutate(bc=mod)
    } else {
        out_df <- df %>%
            dplyr::do(data.frame(
                .,
                bc1=doQmap(.$mod,qm.fit1),
                bc2=doQmap(.$mod,qm.fit2)
            )
            ) %>%
            dplyr::mutate(bc=mod+bc1-bc2) %>%
            dplyr::select(-bc1,-bc2)
    }
	
	return(out_df)
}
