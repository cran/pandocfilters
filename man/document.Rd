\name{document}
\alias{document}
\title{Create a new Document}

\usage{
   document()
}

\description{
   Constructs an object of type \code{"document"}.
}

\details{
   Each document has the following methods:
}

\section{\code{to_json()}}{
   \bold{Description} \cr
   Returns the \code{JSON} representation of the document.
}

\section{\code{write(con, format = "markdown", writer = write.pandoc)}}{
   \bold{Description} \cr
   Write the JSON-formatted AST to a connection. \cr
   \bold{Arguments}
   \tabular{ll}{
      \code{con}    \tab a connection object or a character string to which the document is written. \cr
      \code{format} \tab a character string giving the format (e.g. \code{"latex"}, \code{"html"}). \cr
      \code{writer} \tab an optional writer function, see \link{write.pandoc}.
   }
   \bold{Note} \cr
   Any function with the three arguments \code{x}, \code{con} and \code{format} can be used as writer function.
}

\section{\code{append(x)}}{
   \bold{Description} \cr
   Append a new block to the document. \cr
   \bold{Arguments}
   \tabular{ll}{
      \code{x} \tab block object or list of block objects.
   }
}

\section{\code{append_plain(x)}}{
   \bold{Description} \cr
   For more information about the arguments see \link{Plain}.
}

\section{\code{append_para(x)}}{
   \bold{Description} \cr
   For more information about the arguments see \link{Para}.
}

\section{\code{append_code_block(attr, code)}}{
   \bold{Description} \cr
   For more information about the arguments see \link{CodeBlock}.
}

\section{\code{append_block_quote(blocks)}}{
   \bold{Description} \cr
   For more information about the arguments see \link{BlockQuote}.
}

\section{\code{append_ordered_list(lattr, lblocks)}}{
   \bold{Description} \cr
   For more information about the arguments see \link{OrderedList}.
}

\section{\code{append_bullet_list(lblocks)}}{
   \bold{Description} \cr
   For more information about the arguments see \link{BulletList}.
}

\section{\code{append_definition_list(x)}}{
   \bold{Description} \cr
   For more information about the arguments see \link{DefinitionList}.
}

\section{\code{append_header(x, level=1L, attr=Attr())}}{
   \bold{Description} \cr
   For more information about the arguments see \link{Header}.
}

\section{\code{append_horizontal_rule()}}{
   \bold{Description} \cr
   For more information about the arguments see \link{HorizontalRule}.
}

\section{\code{append_table(rows, col_names=NULL, aligns=NULL, col_width=NULL, caption=list())}}{
   \bold{Description} \cr
   For more information about the arguments see \link{Table}.
}

\section{\code{append_div(blocks, attr)}}{
   \bold{Description} \cr
   For more information about the arguments see \link{Div}.
}

\section{\code{append_null()}}{
   \bold{Description} \cr
   For more information about the arguments see \link{Null}.
}

