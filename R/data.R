#' Candidate list from the 2000 regional election in the Czech Republic
#'
#' A dataset containing candidate lists
#'
#' @format A data frame with 7725 rows and 17 variables:
#' \describe{
#'   \item{row_id}{serial ID of row}
#'   \item{first_name}{First name of the candidate}
#'   \item{last_name}{First name of the candidate}
#'   \item{titles}{Academic titles}
#'   \item{highest_titles}{the highest attained title}
#'   \item{gender}{}
#'   \item{approx_birth_year}{Calculated year of birth using
#'   the year of election minus age of the candidate}
#'   \item{occupation}{Occupation}
#'   \item{residence}{Place of residence of the candidate}
#'   \item{region}{Region}
#'   \item{nominate_party}{Political party which nominated the candidate}
#'   \item{member_party}{Political party which is the candidate member of}
#'   \item{party}{Name of the party}
#'   \item{abs_votes}{Preferential votes the candidate received}
#'   \item{pct_votes}{Percentage share of preferential votes}
#'   \item{rank}{Rank}
#'   \item{mandate}{Mandate}
#'   ...
#' }
#' @source \url{https://volby.cz/}
"kraj_2000"

#' Candidate list from the 2004 regional election in the Czech Republic
#'
#' A dataset containing candidate lists
#'
#' @format A data frame with 8310 rows and 17 variables:
#' \describe{
#'   \item{row_id}{serial ID of row}
#'   \item{first_name}{First name of the candidate}
#'   \item{last_name}{First name of the candidate}
#'   \item{titles}{Academic titles}
#'   \item{highest_titles}{the highest attained title}
#'   \item{gender}{}
#'   \item{approx_birth_year}{Calculated year of birth using
#'   the year of election minus age of the candidate}
#'   \item{occupation}{Occupation}
#'   \item{residence}{Place of residence of the candidate}
#'   \item{region}{Region}
#'   \item{nominate_party}{Political party which nominated the candidate}
#'   \item{member_party}{Political party which is the candidate member of}
#'   \item{party}{Name of the party}
#'   \item{abs_votes}{Preferential votes the candidate received}
#'   \item{pct_votes}{Percentage share of preferential votes}
#'   \item{rank}{Rank}
#'   \item{mandate}{Mandate}
#'   ...
#' }
#' @source \url{https://volby.cz/}
"kraj_2004"

#' Candidate list from the 2008 regional election in the Czech Republic
#'
#' A dataset containing candidate lists
#'
#' @format A data frame with 8207 rows and 17 variables:
#' \describe{
#'   \item{row_id}{serial ID of row}
#'   \item{first_name}{First name of the candidate}
#'   \item{last_name}{First name of the candidate}
#'   \item{titles}{Academic titles}
#'   \item{highest_titles}{the highest attained title}
#'   \item{gender}{}
#'   \item{approx_birth_year}{Calculated year of birth using
#'   the year of election minus age of the candidate}
#'   \item{occupation}{Occupation}
#'   \item{residence}{Place of residence of the candidate}
#'   \item{region}{Region}
#'   \item{nominate_party}{Political party which nominated the candidate}
#'   \item{member_party}{Political party which is the candidate member of}
#'   \item{party}{Name of the party}
#'   \item{abs_votes}{Preferential votes the candidate received}
#'   \item{pct_votes}{Percentage share of preferential votes}
#'   \item{rank}{Rank}
#'   \item{mandate}{Mandate}
#'   ...
#' }
#' @source \url{https://volby.cz/}
"kraj_2008"

#' Candidate list from the 2012 regional election in the Czech Republic
#'
#' A dataset containing candidate lists
#'
#' @format A data frame with 7725 rows and 17 variables:
#' \describe{
#'   \item{row_id}{serial ID of row}
#'   \item{first_name}{First name of the candidate}
#'   \item{last_name}{First name of the candidate}
#'   \item{titles}{Academic titles}
#'   \item{highest_titles}{the highest attained title}
#'   \item{gender}{}
#'   \item{approx_birth_year}{Calculated year of birth using
#'   the year of election minus age of the candidate}
#'   \item{occupation}{Occupation}
#'   \item{residence}{Place of residence of the candidate}
#'   \item{region}{Region}
#'   \item{nominate_party}{Political party which nominated the candidate}
#'   \item{member_party}{Political party which is the candidate member of}
#'   \item{party}{Name of the party}
#'   \item{abs_votes}{Preferential votes the candidate received}
#'   \item{pct_votes}{Percentage share of preferential votes}
#'   \item{rank}{Rank}
#'   \item{mandate}{Mandate}
#'   ...
#' }
#' @source \url{https://volby.cz/}
"kraj_2012"


#' Candidate list from the 2017 parliamentary election in the Czech Republic
#'
#' A dataset containing candidate lists of parties running in the election to
#' the Chamber of Deputies in 2017.
#'
#' @format A data frame with 7540 rows and 17 variables:
#' \describe{
#'   \item{VOLKRAJ}{Region}
#'   \item{KSTRANA}{Code of the political party whose candidate list it is}
#'   \item{PORCISLO}{Position of the candidate on the candidate list}
#'   \item{JMENO}{First name of the candidate}
#'   \item{PRIJMENI}{Last name of the candidate}
#'   \item{TITULPRED}{Academic titles written before the name}
#'   \item{TITULZA}{Academic titles written after the name}
#'   \item{VEK}{age}
#'   \item{POVOLANI}{Occupation}
#'   \item{BYDLISTEN}{Place of residence of the candidate (name)}
#'   \item{BYDLISTEK}{Place of residence of the candidate (id of municipality)}
#'   \item{PSTRANA}{Political party which is the candidate member of}
#'   \item{NSTRANA}{Political party which nominated the candidate}
#'   \item{PLATNOST}{Validity of the candidate (FALSE if candidate retracted
#'   from the election)}
#'   \item{POCHLASU}{Preferential votes the candidate received}
#'   \item{POCPROC}{Percentage share of preferential votes (if more than 5%)}
#'   \item{POCPROCVSE}{Percentage share of preferential votes}
#'   \item{MANDAT}{Mandate (1 if the candidate was elected)}
#'   \item{PORADIMAND}{The order in which the candidate gained the mandate}
#'   \item{PORADINAHR}{The order of substitutes for elected MPs}
#'   ...
#' }
#' @source \url{https://volby.cz/}
"psp_2017"


#' Donations received by ANO in the campaign for the parliamentary election in 2017
#'
#' @format A data frame with 161 rows and 8 variables:
#' \describe{
#'   \item{datum}{Date}
#'   \item{částka Kč}{Amount of donation in CZK}
#'   \item{příjmení}{Last name of the candidate}
#'   \item{jméno}{First name of the candidate}
#'   \item{titul před}{Academic titles written before the name}
#'   \item{titul za}{Academic titles written after the name}
#'   \item{datum narození}{Date of birth}
#'   \item{obec}{Place of residence of the donor (name)}
#'   ...
#' }
#' @source \url{https://udhpsh.cz/}
"ano_donations_2017"

