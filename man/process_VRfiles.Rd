% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_VRfiles.R
\name{process_VRfiles}
\alias{process_VRfiles}
\title{Process VR files from RIC system}
\arguments{
\item{exp}{A character string specifying the experiment ID to use in file paths and processing.}

\item{VRfile}{VRfile with intakes}

\item{bins}{Vector with the number of bins to be used. For example, 'seq(1,32)' or 'c(1,2,3,4,5,6, ..., 32)'}

\item{save_dir}{The directory path where the output files should be saved.}
}
\value{
A plot with the Fed in Kg per animal and an Excel file with the intakes.
}
\description{
Processes VR files from RIC system with feed intakes and
   save the results into an Excel file.
}
