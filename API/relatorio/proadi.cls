\NeedsTeXFormat{LaTeX2e}
\LoadClass{article}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax

\RequirePackage[a4paper,left=2cm,right=2cm,top=3cm,bottom=3cm]{geometry}
\RequirePackage{graphicx}
\RequirePackage{tabularx}
\RequirePackage{amsmath}
\RequirePackage{url}
\RequirePackage[usenames,dvipsnames]{xcolor}
\RequirePackage{float}
\RequirePackage{ragged2e}
\RequirePackage{fancyhdr}
\RequirePackage{tikz}
\RequirePackage[titles]{tocloft}
\RequirePackage{lastpage}
\RequirePackage{titlesec}
\RequirePackage{titletoc}
\RequirePackage[title,titletoc]{appendix}
\RequirePackage{caption}

% Fonte
\RequirePackage[T1]{fontenc}
\fontsize{12}{12}\selectfont
\RequirePackage{fourier}
\urlstyle{same}

% Imagens
\let\origfigure\figure
\let\endorigfigure\endfigure
\renewenvironment{figure}[1][2] {\expandafter\origfigure\expandafter[H]}
{\endorigfigure}

% Cores
\definecolor{cinza}{RGB}{229 229 229}

% \let\oldtextbf\textbf
% \renewcommand{\textbf}[1]{\azul{\oldtextbf{#1}}}

% Base
\newcommand{\base}[1]{ \def \proadi@base{#1} }

\renewcommand{\contentsname}{Sum\'{a}rio}

% Sumário sem pontos
\renewcommand{\cftdot}{}

% Cabeçalho
\pagestyle{fancy}
\fancyhf{}
\rhead{\flushright{\Large PROADI - Hospital Israelita Albert Einstein}\\}
\renewcommand{\headrulewidth}{0cm}
\setlength{\headheight}{2.5cm}
\setlength{\headsep}{0.5cm}
\rfoot{p. \thepage \hspace{1pt} de \pageref{LastPage}}

% Fonte das referências
\renewcommand{\appendixname}{Ap\^{e}ndice}

% Título dos gráficos
\captionsetup[figure]{name={Gr\'{a}fico}}

\newcommand\makeDocument {
    \setlength{\parindent}{0cm}
    % Capa
    \thispagestyle{empty}
    \setlength{\parindent}{0cm}
    {\huge PROADI - Hospital Israelita Albert Einstein}\\
    \begin{tikzpicture}[remember picture, overlay]
	\fill[cinza, opacity=1] (-\paperwidth,-2.0) rectangle (\paperwidth,-\paperheight);
	\end{tikzpicture}
	
	\vspace{6cm}
    \flushright{\Large{Vers\~{a}o gerada automaticamente}}
    \newpage
    
    \thispagestyle{empty}
    \setlength{\parindent}{0cm}
    \flushleft
    {\huge PROADI - Hospital Israelita Albert Einstein}\\
    \begin{tikzpicture}[remember picture, overlay]
	\fill[cinza, opacity=1] (-\paperwidth,-2.0) rectangle (\paperwidth,-\paperheight);
	\end{tikzpicture}
	
	\vspace{6cm}
    \LARGE{\textbf{Qualidade de dados}}

	\Large{\MakeUppercase{\proadi@base}}
    \newpage

    % Sumário
    \tableofcontents
    \newpage
    
    \justifying
    \setlength{\parindent}{20pt}
}
\AtBeginDocument{\makeDocument}