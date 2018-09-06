#' Admission episode data for Northwick Park and Central Middlesex hospitals
#'
#' A dataset containing data on 807,603 episodes of inpatient care at Northwick
#' Park and Central Middlesex hospitals, for all patients discharged between
#' 2012 and 2016.
#'
#' @format A data frame with 807,603 observations of 30 variables. Each
#' observation corresponds to an episode of care.
#' \describe{
#'   \item{PseudoID}{Unique ID number of the patient. Original mapping back to
#'   hospital number remains behind North West London Hospitals NHS firewall}
#'   \item{AgeBand}{The age band that the patient was in at time of admission}
#'   \item{AdmissionDate}{The date of admission for the spell in hospital that
#'   this episode of care is part of}
#'   \item{CSPAdmissionTime}{The time that the patient was admitted}
#'   \item{DischargeDate}{The date of discharge for the spell in hospital that
#'   this episode of care is part of}
#'   \item{CSPDischargeTime}{The time that the patient was discharged from
#'   hospital}
#'   \item{PrimaryDiagnosis}{The primary diagnosis for this episode as an
#'   ICD-10 code}
#'   \item{SecondaryDiagnosis1}{The first secondary diagnosis for this episode,
#'   as an ICD-10 code. Subsequent fields document additional diagnoses,
#'   numbered 2 - 9}
#'   \item{LSOA}{The lower super output area of the patient's home postcode}
#'   \item{Postcode.Sector}{The postcode of the patient's home, up to sector
#'   level}
#'   \item{EthnicGroup}{The ethnicity of the patient recorded using the NHS data
#'   dictionary coding system
#'   \url{http://www.datadictionary.nhs.uk/data_dictionary/attributes/e/end/ethnic_category_code_de.asp}}
#'   \item{Sex}{The gender of the patient}
#'   \item{HRGV4Spell}{The spell level HRG4 code}
#'   \item{EpisodeNumber}{This is the number of the episode within the spell
#'   counting consecutively from the first}
#'   \item{EpisodeStartDate}{The date on which this episode began}
#'   \item{EpisodeStartTime}{The start time of this episode}
#'   \item{EpisodeEndDate}{The date on which this episode ended}
#'   \item{EpisodeEndTime}{The end time of this episode}
#'   \item{AdmissionMethodCode}{The method of admission coded using the NHS data
#'   dictionary coding system
#'   \url{http://www.datadictionary.nhs.uk/data_dictionary/attributes/a/add/admission_method_de.asp}}
#'   \item{AdmissionType}{Mapping of admission level code to the higher level
#'   categories Elective, Emergency, Maternity, Other}
#'   \item{CSPLastWard}{The ward from which the patient was discharged}
#'   \item{TreatmentFunctionCode}{The treatment function code for the episode,
#'   coded using the NHS data dictionary coding system
#'   \url{http://www.datadictionary.nhs.uk/data_dictionary/attributes/t/tran/treatment_function_code_de.asp?shownav=1}}
#'
#'
#'
#' }
"admission_data"


#' Admission spell data for Northwick Park, Central Middlesex and Ealing hospitals
#'
#' A dataset containing data on 310,629 spells of inpatient care at Northwick
#' Park, Central Middlesex and Ealing hospitals, for all patients admitted as
#' an emergency and discharged between 2012 and 2017.
#'
#' @format A data frame with 310,629 observations of 39 variables. Each
#' observation corresponds to a spell in hospital.
#' \describe{
#'   \item{PseudoID}{Unique ID number of the patient. Original mapping back to
#'   hospital number remains behind North West London Hospitals NHS firewall}
#'   \item{AgeBand}{The age band that the patient was in at time of admission}
#'   \item{DateOfDeath}{If the patient died in hospital, the date of death, otherwise NA}
#'   \item{CSPAdmissionTime}{The date and time that the patient was admitted}
#'   \item{CSPDischargeTime}{The date and time that the patient was discharged from
#'   hospital}
#'   \item{LSOA}{The lower super output area of the patient's home postcode}
#'   \item{Sex}{The gender of the patient}
#'   \item{HRGV4Spell}{The spell level HRG4 code}
#'   \item{AdmissionMethodCode}{The method of admission coded using the NHS data
#'   dictionary coding system
#'   \url{http://www.datadictionary.nhs.uk/data_dictionary/attributes/a/add/admission_method_de.asp}}
#'   \item{AdmissionType}{Mapping of admission level code to the higher level
#'   categories Elective, Emergency, Maternity, Other}
#'   \item{CSPLastWard}{The ward from which the patient was discharged}
#'   \item{TreatmentFunctionCode}{The treatment function code for the episode,
#'   coded using the NHS data dictionary coding system
#'   \url{http://www.datadictionary.nhs.uk/data_dictionary/attributes/t/tran/treatment_function_code_de.asp?shownav=1}}
#'   \item{DischargeMethodCode}{The discharge method code, as per NHS data dictionary}
#'   \item{DischargeDestinationCode}{The discharge destination code as per NHS data dictionary}
#'   \item{EthnicGroupComp2}{An alternative compression of the ethnic group coding}
#'   \item{EthnicGroupComp}{A compression of the ethnic group code into the high level categories found here
#'   \url{http://www.datadictionary.nhs.uk/data_dictionary/attributes/e/end/ethnic_category_code_de.asp},
#'   i.e. W = White, X = Mixed, Y = Asian/Asian British, U = Black/Black British, V = Other}
#'   \item{Heart.Failure.Episode}{TRUE iff primary diagnosis Heart Failure}
#'   \item{HF.any.code}{TRUE iff at least one diagnosis code Heart Failure (primary, secondary, ...)}
#'   \item{Bundle.Number}{If there is a care bundle associated with this spell, the id number of that bundle}
#'   \item{nicor.entry.id}{If there is a NICOR record associated with this spell, the id number of that record}
#'   \item{bundle}{TRUE iff there is a care bundle associated with this spell}
#'   \item{nicor}{TRUE iff there is a NICOR record associated with this spell}
#'   \item{IMD}{The index of multiple deprivation for the patients' home postcode}
#'   \item{Age.est}{Mid-point of the age category this patient fell within at admission}
#'   \item{AgeBand_B}{Binary classification of patients by age: young = <65, old = >=65}
#'   \item{died}{TRUE if the patient died in hospital}
#'   \item{AdmissionMonth}{Month in which this spell began}
#'   \item{DischargeMonth}{Month in which this spell ended}
#'   \item{next.adm.dt}{The date of the next (chronological) spell for this patient, NA if no such spell}
#'   \item{time.to.next.spell}{The duration from the discharge date of this spell to the admission date of the next,
#'   if there is one, NA otherwise.}
#'   \item{time.to.next.hf.spell}{The duration from the discharge date of this spell to the admission date of the next
#'   Heart Failure spell, if there is one, NA if not.}
#'   \item{all.cause.readmission.cat}{The readmission category for this spell - 7 = another spell within 7 days,
#'   30 = ... within 30 days, 90 = ... within 90 days, NA = longer than 90 or no subsequent spell.}
#'   \item{hf.readmission.cat}{The Heart Failure specific readmission category for this spell.
#'   As per all-cause but second spell restricted to HF only.}
#'
#' }
"emergency_adms"
