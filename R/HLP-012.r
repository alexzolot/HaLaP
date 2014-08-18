# Miscellaneous aliases and functions
# Author   : Alex Zolotoviski, alex@zolot.us
# Created  : 2013-06-24 02:55:55
# License  : GPL-2  
#' @name HLP
#' 
#' @title HLP  - Miscellaneous aliases and functions
#' 
#' @description HLP  - Miscellaneous aliases and functions that I use the most often 
#' during project initiation, in progress, leaving and coming back
#' for a book "Handling Large R Projects"
#' 
# abbreviations: ds - dataset, patt - pattern, pattNeg - negative pattern,  #e - example 
#' @author Alex Zolotovitski <alex@@zolot.us>
# @Authors@R  Alex Zolotovitski <alex@@zolot.us>
#' @keywords aliases

#' @exportPattern "^[[:alpha:]]+"
#' @export '%+%' '%-%'

#' @import data.table SOAR  
#' @docType package
NULL


#' abbreviations: ds - dataset, patt - pattern, pattNeg - negative pattern,  #ex: ,  #e  - example ,  #en  - example don't run

#=====  Funcs for book HLRP  =====
#     General purpose helper functions and aliases     

# xxx : saa, sa() -> sa()

#    aliases and 1-liners    
ch= as.character
fa= factor
nu= as.numeric
df= data.frame

# uncomment the next 2 lines, if run by source()
 stopifnot(require(data.table))
 stopifnot(require(SOAR)) 
dtt= data.table; ad= as.IDate; taa= tables  # vignette("datatable-faq")
sto= srm= Store

le= length
he= head
sus= subset
summ= summary
sf= sprintf
na= names
fp= file.path
fpa= tools:::file_path_as_absolute
#fpa= function(f)normalizePath(path.expand(f), "/", TRUE)

brr= browser  #function(s='', ...){catt(s); browser(...)}   
trb= traceback

#' fun to split column names
#' @examples 
#'   cn('aa bb 2013')
#' @title cn
#' @aliases cn 
cn= function(cnn,  sep='[ , \n\t\\+]+') unlist(strsplit(cnn, sep))  # b='', `%a%` = '%c%' =  col.names= cn(cnn)  'aa b 1'  %a% 1  aa b 1'  cn('aa b 1')

#' do nothing - when we need temporary disable a function f,  use f= dummy  
#' @title dummy
#' @aliases dummy 
dummy= function(...) invisible()

#' fun to paste column names,  inverse to cn()
#' @examples 
#'  nc(cars)
#' @title nc= nmsv= sNames
#' @aliases nc nmsv sNames 
nc= nmsv= sNames= function(ds, nm= deparse(substitute(ds)), sep=' ', ...){x=pas(na(ds), sep, sep); catf("\nnames(%s)= cn('%s')\n", nm, x); invisible(x)} 

#' wrapper for subset + paste + colNames
#' @examples 
#'  ncc('Wi', , iris)
#' @title ncc
#' @aliases ncc 
ncc= function(patt = "", pattNeg = "^$", ds) pas(gre2(patt, pattNeg, na(ds)))
#' @examples 
#'  pr(cars)
#' @title pr
#' @aliases pr 
pr= function(x){catf('==  %s  ==\n', deparse(substitute(x))); print(x)} 

#' print list in 1 column
#' @examples 
#'  prr(letters[1:5])
#' @examples 
#'  prr(cars); prr(dir()); prr(character(0));
#' @title prr
#' @aliases prr 
prr= function(x, ma='', header=TRUE) { if(header)catf('\n&&& %s == %s ==\n', ma, deparse(substitute(x))); ns= na(x)
		if(le(x)==0){message('length=0'); return(0)}
     	for(i in if(is.null(ns)) 1:le(x) else ns) catf('%3s= %s\n', i,  x[[i]]); catt('-------------------------\n')
 	} 

#' wrapper for for paste
#' @title pas
#' @aliases pas 
pas= function(x, sep=' ', collapse=' ')paste(x, sep=sep, collapse=collapse)
#' wrapper for grep
#' @title gre2
#' @aliases gre2 
gre2= function(patt='', pattNeg='^$', x, v=TRUE, ...){ a=grepl(patt, x, ...) & !grepl(pattNeg, x, ...); return( if(v) x[a] else a) }
#' wrapper for dir
#' @title dir2
#' @aliases dir2 
dir2= function(patt='', pattNeg='^$', ...) gre2(patt, pattNeg, dir(...))

#' wrapper for str
#'e strr(cars)
#' @title strr
#' @aliases strr 
strr= function(x) {catf('\nstr(%s):\n', desu(x)); str(x)}


#' wrapper for for grep + names + subset
#' @examples 
#'  suss('Se', 'Wi',  iris, Sepal.Length < 4.5 )
#' @title suss
#' @aliases suss 
suss= function(patt='', pattNeg='^$', x, ...) {
	cols= grepl(patt, colnames(x)) & !grepl(pattNeg, colnames(x))  # gna(patt, pattNeg, x)  
	if('data.table' %in% class(x)) x[...][, cols, with=FALSE] else subset(x, ...)[cols]
}
if (0) {
	strr(freeny)
	strr(dtt(freeny))
	suss(, , freeny)
	suss(, , dtt(freeny))
	suss('in', ,  freeny, y < 8.9)
	suss('in', ,  freeny, y < 8.9)
	suss(, 'in', freeny, y < 8.9)
	suss(, 'in', dtt(freeny), y < 8.9)
	suss( 'in', 'p', dtt(freeny), y < 8.9)	
}



#' prepare to detach redundant packages
#' @title dett
#' @aliases dett 
dett= function(patt= '^pac') catf('\ndetach("%s",  character.only = TRUE)', grep(patt, search(), v=TRUE))  
#' system time + sound
#' @examples 
#'  st({x= 5})
#' @title st
#' @aliases st 
st= function(...){s= system.time(...)[[3]]; aaa(); s}  

#' wrapper for nu * fa
# nuf(ch(CO2$Plant))
# nuf(ch(iris$Species))
#' @title nuf
#' @aliases nuf 
nuf= function(...) nu(fa(...))  # char -> fa -> nu

#' compare names
#' @examples 
#'  comp.na(cars, mtcars)
#' @title comp.na
#' @aliases comp.na 
comp.na= function(x, y) {
	catt('x & y :', pas(intersect(na(x), na(y))))
	catt('x \\ y :', pas(setdiff(na(x), na(y))))
	catt('y \\ x :', pas(setdiff(na(y), na(x))))  # nin
}


onWin= nchar(Sys.getenv('computername')) > 0
#ww= if(onWin)windows  else x11

#' @title ww
#' @aliases ww 
ww= function(k=10, ...) if(onWin){
			#ldply(strsplit(system("wmic desktopmonitor get", intern=TRUE), '\r'))
			# 1   Availability  Bandwidth  Caption                    ConfigManagerErrorCode  ConfigManagerUserConfig  CreationClassName     Description                DeviceID         DisplayType  ErrorCleared  ErrorDescription  InstallDate  IsLocked  LastErrorCode  MonitorManufacturer       MonitorType                Name                       PixelsPerXLogicalInch  PixelsPerYLogicalInch  PNPDeviceID                               PowerManagementCapabilities  PowerManagementSupported  ScreenHeight  ScreenWidth  Status  StatusInfo  SystemCreationClassName  SystemName  
			# 2 3                        ThinkPad Display 1600x900  0                       FALSE                    Win32_DesktopMonitor  ThinkPad Display 1600x900  DesktopMonitor1                                                                                     Lenovo                    ThinkPad Display 1600x900  ThinkPad Display 1600x900  96                     96                     DISPLAY\\LEN40B1\\4&2C1ABB3B&0&UID67568640                                                         1080          1920         OK                  Win32_ComputerSystem     T530-1122   
			# 3 3                        Generic PnP Monitor        0                       FALSE                    Win32_DesktopMonitor  Generic PnP Monitor        DesktopMonitor2                                                                                     (Standard monitor types)  Generic PnP Monitor        Generic PnP Monitor        96                     96                     DISPLAY\\SAM0686\\5&24EFB498&0&UID1048850                                                          1080          1920         OK                  Win32_ComputerSystem     T530-1122   
		prr(system("wmic desktopmonitor get",  intern=TRUE))
#' @title 	winprop
#' @aliases 	winprop 
	winprop= function(s='screenwidth') max(nu(system(sf("wmic desktopmonitor get %s", s),  intern=TRUE)), na.rm=TRUE) #[2]
			
	windows( width= k/10 * max(winprop('screenwidth'),  1920)/ winprop('PixelsPerXLogicalInch')
		  , height= k/10 * max(winprop('screenheight'), 1080)/ winprop('PixelsPerYLogicalInch'), ...)
	}  else x11


#' date &  time 
#' @examples 
#'  DT()
#' @title DT= DateTime
#' @aliases DT DateTime 
DT= DateTime= function(format = "%Y-%m-%d %H:%M:%S") strftime(Sys.time(), format) 

#' wrapper for dim + head
#' @examples 
#'  hee(cars)
#' @title hee
#' @aliases hee 
hee= function(ds, h=9){catf('\n%s, %s rows x  %s cols,  %s Mb :\n', deparse(substitute(ds)), NROW(ds), NCOL(ds), round(object.size(ds)/2^20, 1)); print(hh<-head(ds, h)); catf('# he(suss(, , %s[ , cn("%s")]), 5)\n', deparse(substitute(ds)), nmsv(ds, deparse(substitute(ds))));invisible(hh)}

#' play sound (after long count)
#' @title aaa
#' @aliases aaa 
aaa= function(n=20) for(i in 1:n) {cat('\aaa '); flush.console()}  # sound when done 

#' wrapper for cat + sprintf
#' @title catf
#' @aliases catf 
catf= function(...) cat(sprintf(...))

#' wrapper for cat + '\\n'
#' @examples 
#'  catt(cars[, 1])
#' @title catt
#' @aliases catt 
catt= function(...) {cat(...); cat('\n'); invisible(flush.console())} 

#'  catt with names
#e	z= 1:5; v= letters[1:2];  catn(z, v, 7, u<-'a', v=88)
#' @title catn
#' @aliases catn 
catn= function(...) { catt('\n__')  # catn:')
	nargs= unlist(strsplit(ch(match.call()), '(), ', fixed =TRUE))[-1]  # names of args
	#for (i in 1:le(nargs)) catt(nargs[[i]],  '= ', list(...)[[i]])
	for (i in 1:le(nargs)) catf('%15s= |%s|\n', nargs[[i]], list(...)[[i]])
}

#' wrapper for paste
'%+%' = paste0 #function(x, y) paste(x, y, sep= "")

#' wrapper for shell
#' @title exec
#' @aliases exec 
exec= function(s) shell(s, wait=TRUE, intern = TRUE)

#' wrapper for  browseURL  or  shell + start explorer 
#' @examples 
#'  expl()
# expl= function(x= gw()) shell(sf('start explorer %s', gsub('/', '\\\\', x)))  # win only
#' @title expl
#' @aliases expl 
expl= function(x= gw(), ...) browseURL(x, ...)

#' wrapper for getwd
#' @examples 
#'  gw()
#' @title gw
#' @aliases gw 
gw= function(){catf('gw: sw("%s");  expl()\n', gw<- getwd()); invisible(gw)} 

#' wrapper for dir.create + setwd
#' @examples 
#'  \dontrun{
#'  sw('myTestDir/mySubdir');  sw('../..'); expl()
#' }
#' @title sw
#' @aliases sw 
sw= function (sDir, ...) {
	dir.create(sDir, rec=TRUE, ...)
	setwd(sDir)
	catf('sw: Work dir set to: %s;  gw()\n', gw())
}


#' x \ y
#r  x  which are not in  y
#' @examples 
#'   nin(1:6, 4:9);  1:6 %-% 4:9
nin= '%-%' = setdiff  # function(x, y) x[!(x %in% y)] # x not in y :   1:5 %-%  4:9 # `%-%` 


#' returns matrix of memory consumption
#' @examples 
#'  x= lss(); lss(0)
#' @title lss
#' @aliases lss 
lss= function(minsize=.2, verb=TRUE){ #object.sizes <-
	llss= dtt(o=ls(envir=.GlobalEnv))[
			, ':='(Mb=round(nu(object.size(g<-get(o, envir=.GlobalEnv)))/1048576, 2)
	        , sto= sf("srm(%19s)", o), cl=pas(class(g)), nr=NROW(g), nc=NCOL(g)), by=o][order(Mb)] 
	
	if(verb)try({
				ww(); pie(llss$Mb, labels = llss$o, main=sf("Memory usage by object, of tot %s Mb", memory.size())) #round(llss/1e6, 2))
				}, s=TRUE)
	#print(llss[1])    #print(llss)     
	do.call(strr, list(get(llss$o[1]) ))
	catf(':: rm(%s)\n', pas(llss$o, collapse=', '))
	llss[Mb>= minsize] 
}


#' list of data.frames 
#' @examples 
#'  lsd(FALSE); a= lsDF(TRUE); hee(a); if(nrow(a)>0)srt(a, ~  + class + ds - size)
#' @title lsd= lsDF
#' @aliases lsd lsDF 
lsd= lsDF= function(.all=FALSE, ...){ 
	b= dtt(o=ls(envir= .GlobalEnv, ...))[, ':='(ds=sf('srm(%20s)', o)
						, cla= class(a<-get(o, envir= .GlobalEnv))[[1]]
						, size=nu(object.size(a))/1e6, nr=NROW(a), nc=NCOL(a)
						, vars=substr(pas(na(a)), 1, 99), isl=is.list(a)), by=o]		
	if(!.all) b= b[isl | cla%in% c('data.frame', 'data.table', 'matrix', 'list', "metaMDS", "itemsets"
								   , "rules", "transactions") ]
	gc(TRUE, TRUE)		   
	b[order(-size), list(ds, cla, size, nr, nc, vars)]
}


#' rm all
#' @title rmall
#' @aliases rmall 
rmall= function() rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv) # rmall()

#' rm  all Data Frames, lists, matrixes
#' @examples 
#'  rmDF()
#' @title rmDF
#' @aliases rmDF 
rmDF= function(...){dtt(o=ls(envir =.GlobalEnv, ... ))[, {aa= get(o);  cl=class(aa)[1]
						if(cl  %in% c('list')){catf('rm  %-20s %-30s\n', o, sNames(aa)); do.call(rm, list(o), envir =.GlobalEnv)}
						if(cl %in% c('data.table', 'data.frame', 'matrix')){catf('rm  %-20s %5s %3s %-30s\n', o, nrow(aa), ncol(aa), substr(sNames(aa), 1, 4999))
							do.call(rm, list(o), envir= .GlobalEnv)
						}}, by=o]
		gc(TRUE, TRUE)
	}

#' install + library
#' @param gh github username 
#' @title libra
#' @aliases libra 
libra= function(libs, verb=TRUE, gh=''){
	libs= if(!grepl('\\"|\'| ', li<- deparse(substitute(libs)))) li else cn(libs)  # now libs is a char vector
	
	misspk= nin(libs, installed.packages()[, 1])
	
	if(le(misspk) > 0) {
		if(gh==''){install.packages(misspk, repos= "http://cran.stat.ucla.edu/", dependencies= TRUE)
		} else {if(le(misspk) > 1) message('we can load only one github package!')
				libra(devtools)
			    install_github(misspk[1], gh, dependencies= TRUE, ...)
		}
	}
	
	for(li in libs){catt('libra:: Call library ', li) 
		do.call(library, list(li, character.only =TRUE))
		if(verb) catf('libra:: demo(%s); example(%1$s); vignette("%1$s")\n', li)
	}
} #--


#' install_github + library
#' @param gh github username 
#' @title libgh
#' @aliases libgh 
libgh= function(libs, verb=TRUE, gh='alexzolot', ...){
	libs= if(!grepl('\\"|\'| ', li<- deparse(substitute(libs)))) li else cn(libs)  # now libs is a char vector
	
	misspk= nin(libs, installed.packages()[, 1])
	
	if(le(misspk) > 0) {
		libra(devtools)
		install_github(misspk[1], gh, dependencies= TRUE, ...)}
	
	for(li in libs){catt('libra:: Call library ', li) 
		do.call(library, list(li, character.only =TRUE))
		if(verb) catf('libra:: demo(%s); example(%1$s); vignette("%1$s")\n', li)
	}
} #--



if(0){ # examples 
	libra(libs= "locfit tkrplot xtable")
	libra(cn("randomForest varSelRF"), verb=TRUE)
	libra("randomForest varSelRF", verb=TRUE)
	libra(randomForest, verb=TRUE)
	
	.libPaths()
	library()
	pas(dir(.libPaths()))
	# [1] "abind akima bak base bitops boot brew Cairo caTools class cluster codetools colorspace datasets DBI Defaults devtools dichromat digest evaluate fImport foreach forecast formatR fracdiff gam gdata ggplot2 gmodels gplots graphics grDevices grid gtable gtools httpuv httr iterators itertools KernSmooth knitr labeling lattice lme4 locfit markdown MASS Matrix memoise methods mgcv munsell nlme nnet plyr png proto quadprog R2HTML randomForest rCharts RColorBrewer Rcpp RcppArmadillo RCurl reshape2 rj rj.gd rJava RJDBC RJSONIO roxygen2 rpart RUnit scales shiny stats stats4 stringr survival testthat timeDate timeSeries tkrplot tools tseries utils whisker XLConnect xtable yaml zoo"
	
	pas(options('defaultPackages')[[1]])
	# [1] "datasets utils grDevices graphics stats methods"
}


#'==   output - input  ==

#' save object or all and print reminder
#' p dsn name for out file
#' @examples 
#'  	
#' \dontrun{
#'	saa(, '111')
#'# 2013-06-18 12:15:31:: Saved: load('m:/80_ChurnSim/out/111.RData'); sw('m:/80_ChurnSim/out')  # rmDF(); lsDF(); dir(); expl()
#'	
#'	saa()
#'# 2013-06-18 12:15:38:: Saved: load('m:/80_ChurnSim/out/.RData'); sw('m:/80_ChurnSim/out')  # rmDF(); lsDF(); dir(); expl()
#'	
#' data(cars); saa(cars)
#' }
#' @title saa
#' @aliases saa 
saa= function(ds, dsn= deparse(substitute(ds)) , ...){file= sf('%s.RData', dsn)
	save(list = if(missing(ds)) ls(all=TRUE) else  dsn, file=file, ...); 
	catf("%s::  %s  saved: lo('%s/%s'); sw('%3$s')\n", Sys.time(), dsn, getwd(), file)
	if(missing(ds)) catt('  # rmDF(); lsDF(); dir(); expl()')
} 

#' wrapper for save.image
#' @examples 
#'  \dontrun{
#'  sa()
#' }
#' @examples 
#'  \dontrun{
#'  sa(2)
#' }
#' @title sa
#' @aliases sa 
sa= function(file=''){save.image(file=sf('%s.RData', file)); catf("%s:: Image saved: lo('%s/%s.RData'); sw('%2$s')  # rmDF(); lsDF(); dir(); expl()\n", Sys.time(), gw(), file)}
#' @title lo
#' @aliases lo 
lo= function(file='.RData'){catt('Loaded:', ll<- pas(load(file=file, .GlobalEnv))); lss(); cat('\a\a\a'); alarm();ll} # expl("C:/Users/Public/Music/Sample Music/Kalimba.mp3")} # expl("Z:/exe/testVoice.vbs")}


#' wrapper for save or save.image
#' @param ... 0, 1, or 2 args
#' 0 - save.image
#' 1 - if char with  le=1 it is file , else object to save
#' 2 - first is obj, second is file name
#en	sa2()	        # 2014-04-04 16:22:51:: Image saved: lo('m:/50_HLP/out/.RData'); sw('m:/50_HLP/out')  # rmDF(); lsDF(); dir(); expl()
#en	sa2('z')	    # 2014-04-04 16:25:07:: Image saved: lo('m:/50_HLP/out/z.RData'); sw('m:/50_HLP/out')  # rmDF(); lsDF(); dir(); expl()
#en	sa2(cars)	    # 2014-04-25 17:07:03::  cars  saved: lo('m:/50_HLP/out/HLP_demo/out/cars.RData'); sw('m:/50_HLP/out/HLP_demo/out')  # rmDF(); lsDF(); dir(); expl()
#en	sa2(cars, 'z') 	# 2014-04-25 17:07:22::  cars  saved: lo('m:/50_HLP/out/HLP_demo/out/z.RData'); sw('m:/50_HLP/out/HLP_demo/out')  # rmDF(); lsDF(); dir(); expl()
#e	gw()
#e	loo()	
#' @title sa2= saaa
#' @aliases sa2 saaa 
sa2= saaa= function(...){
	dots <- list(...)                  
	ar= deparse(substitute(...))
	#strr(ar); strr(dots); catt('na(dots)=', na(dots), class(last(dots)), '====\n'); # return(invisible(NULL))
	
	if(0){
		file=  if(le(dots) == 0) '' else if(class(dots[[1]])=='character' &&  le(dots[[1]])==1) dots[[1]] else if(le(dots) == 1) ar[1] else dots[[2]]
		catf('file=%s.Rdata===\n', file)
	}
	
	if(le(dots) == 0){save.image(file='.RData'); catf("%s:: Image saved: lo('%s/.RData'); sw('%2$s')  # rmDF(); lsDF(); dir(); expl()\n", Sys.time(), gw())
	} else if(le(dots)==1 && le(dots[[1]])==1 && class(dots[[1]])=='character'){
		file= dots[[1]]
		save.image(file=sf('%s.RData', file)); catf("%s:: Image saved: lo('%s/%s.RData'); sw('%2$s')  # rmDF(); lsDF(); dir(); expl()\n", Sys.time(), gw(), file)
	#} else if(le(dots)==1  && NROW(dots[[1]]) >1 ) {  
	} else if(le(dots)==1  && class(dots[[1]])!= 'character' ) {  
		file= sf('%s.RData', ar[[1]])
		save(list=ar, file=file); 
		catf("%s::  %s  saved: lo('%s/%s'); sw('%3$s')  # rmDF(); lsDF(); dir(); expl()\n", Sys.time(), ar[[1]], getwd(), file)
	#} else if(le(dots)==2 && NROW(dots[[2]])==1 && class(dots[[2]]) =='character'){
	} else if(le(dots)==2 && class(dots[[2]]) =='character'){
		x= deparse(substitute(dots[[1]]))
		file= sf('%s.RData', dots[[2]])
		save(list=ar[[1]], file=file); 
		catf("%s::  %s  saved: lo('%s/%s'); sw('%3$s')  # rmDF(); lsDF(); dir(); expl()\n", Sys.time(), ar[[1]], getwd(), file) 
	} else {message('Wrong args');  strr(ar); strr(dots)}
}



#' list of ../out/.RData files 
#' @examples 
#'   loo()
#' @title loo
#' @aliases loo 
loo= function(patt='.RData', d= gw()){ gw(); 
	b= dtt(f= dir(d, patt=patt, all.files =TRUE, re=TRUE))[, ff:= fp(d, f)]
	if(nrow(b) > 0) { b[, ':='(mtime=ch(file.info(ff)$mtime), size=round(file.info(ff)$size/1e6, 1)
						, lo=sf('lo("%s")', ff)), by=f][order(mtime)]
	} else warning(sf('no %s files in the directory %s', patt, d))
	b[, ff:=NULL]
}	



##' save & rm()
##en ca= cars; srm(ca)	# creates ca.RData
#srm= Store  # function(x) {saa(x, dsx<- deparse(substitute(x)) ); rm(list= dsx, envir = .GlobalEnv); catf('\n!!!    %s  is saved & removed  !!!\n', dsx)}


#' save an object to .csv
#' wrapper for for write.csv 
#' @title tocsv
#' @aliases tocsv 
tocsv= function(ds, dsn= sf('%s.csv', deparse(substitute(ds))), ...){ catt(dsn); write.csv(ds, file=dsn, quote=FALSE, row.names= FALSE); 
	catf("Saved: %s= read.csv('%s/%s'); expl('%2$s/%3$s')\n ", deparse(substitute(ds)), gw(), dsn)} 
#' @title totsv
#' @aliases totsv 
totsv= function(ds, dsn= sf('%s.tsv', deparse(substitute(ds))), ...){ write.table(ds, file=dsn, quote=FALSE, row.names= FALSE, sep='\t', ...); 
	catf("Saved: %s= read.delim('%s/%s'); expl('%2$s/%3$s')\n ", deparse(substitute(ds)), gw(), dsn)} 


#' Sort Data Frame
	#' Author: Kevin Wright
	#' with some ideas from Andy Liaw
	#' http://tolstoy.newcastle.edu.au/R/help/04/07/1076.html
	
	#p x: A data.frame
	#p by: A one-sided formula using + for ascending and - for descending
	#'     Sorting is left to right in the formula
	
	#' Useage is:
	# srt(cars, by= ~speed - dist)
#' @title srt= sortt= sort.data.frame
#' @aliases srt sortt sort.data.frame 
srt= sortt= sort.data.frame= function(x, by){

	
	if(by[[1]] != "~")
		stop("Argument 'by' must be a one-sided formula.")
	
	# Make the formula into character and remove spaces
	formc <- as.character(by[2]) 
	formc <- gsub(" ", "", formc) 
	# If the first character is not + or -, add +
	if(!is.element(substring(formc, 1, 1), c("+", "-")))
		formc <- paste("+", formc, sep = "")
	
	# Extract the variables from the formula
	vars <- unlist(strsplit(formc, "[\\+\\-]"))    
	vars <- vars[vars != ""] # Remove any extra "" terms
	
	# Build a list of arguments to pass to "order" function
	calllist <- list()
	pos <- 1 # Position of + or -
	for(i in 1:length(vars)){
		varsign <- substring(formc, pos, pos)
		pos <- pos + 1 + nchar(vars[i])
		if(is.factor(x[, vars[i]])){
			if(varsign == "-") {
				calllist[[i]] <- -rank(x[, vars[i]])
			} else {
				calllist[[i]] <- rank(x[, vars[i]])
			}
		} else {
			if(varsign == "-") {
				calllist[[i]] <- -x[, vars[i]]
			} else {
				calllist[[i]] <- x[, vars[i]]
			}
		}
	}
	return(x[do.call("order", calllist), ])
}



if(0){   #== Misc
	libra(installr); updateR()
	theFile= 'm:/50_HLP/out/packages/HLP.r'
	theFile= 'm:/50_HLP/out/packages/HaLaP/inst/rcode/HLP.r'
	gff('saved', theFile)
	gff('sa\\(|===', theFile)
	gff('\\bF\\b', theFile)
	
	theFile= fp(proot, '50_HLP.r')
	
	CreateNewProj(newProj.name= 'zzz', Templ.dir= 'TRUE:/work/UseR-2013/lib/newProjTemplNa me', root='TRUE:/work/UseR-2013')
	CreateNewProj(newProj.name= '49_2048', Templ.dir= 'TRUE:/work/UseR-2013/lib/newProjTemplName', root='m:')
} #--




