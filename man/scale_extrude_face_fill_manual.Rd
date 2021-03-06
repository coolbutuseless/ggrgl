% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale-extrude-manual.R
\name{scale_extrude_face_fill_manual}
\alias{scale_extrude_face_fill_manual}
\alias{scale_extrude_face_alpha_manual}
\alias{scale_extrude_edge_colour_manual}
\alias{scale_extrude_edge_alpha_manual}
\title{Create your own discrete scale}
\usage{
scale_extrude_face_fill_manual(
  ...,
  values,
  aesthetics = "extrude_face_fill",
  breaks = waiver()
)

scale_extrude_face_alpha_manual(
  ...,
  values,
  aesthetics = "extrude_face_alpha",
  breaks = waiver()
)

scale_extrude_edge_colour_manual(
  ...,
  values,
  aesthetics = "extrude_edge_colour",
  breaks = waiver()
)

scale_extrude_edge_alpha_manual(
  ...,
  values,
  aesthetics = "extrude_edge_alpha",
  breaks = waiver()
)
}
\arguments{
\item{...}{arguments passed on to \code{discrete_scale}. See \code{ggplot2::scale_fill_manual}
for more details.}

\item{values, aesthetics, breaks}{See \code{ggplot2::scale_fill_manual}
documentation for more details.}
}
\description{
These functions allow you to specify your own set of mappings from levels
in the data to aesthetic values.
}
