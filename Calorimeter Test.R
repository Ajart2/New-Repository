##############################################################################
#Reset R and Load Libraries
##############################################################################
rm(list=ls())
cat("\014")

library(EmersonDataScience)
library(devtools)

##############################################################################
#Copy of Test Data Sheet
##############################################################################

#Test Data
#     Test Date                   4-17-14
#     Test Time                   16:30
#     Project Number              NA
#     Plant Number                1
#     Room Number                 PELAB007
#     ELT Number                  TS0042-008
#     Log Number                  2
#     Technician                  BORGES

#Compressor Data
#     Model Number                ZFV046-1E9-FBT
#     Serial Number               14DE24533
#     Motor MFG                   EHMD
#     Protector MFG               -X-
#     Displacement                2.8
#     Volatage                    460
#     Frequency                   60
#     Phase                       3
#     Run Capacitor               -X-
#     Oil Type                    3MAF POE

#Test Point
#     Refrigerant                 R-404A
#     Evaporating Temperature     20.0        °F (70.0    PSIA)
#     Condensing Temperature      90.0        °F (217.11  PSIA)
#     Suction Vapor Superheat     45.0        °F
#     Liquid Sub-Cooling          0.0         °F
#     Ambient Temperature         95.0        °F

#Performance Data
#     CMF                         464.27
#     Mass Flow                   468.99      LBM/HR
#     Capacity                    28068.0     BTU/HR
#     Energy Efficiency           13.07       Ratio
#     Voltage                     287.3       Volts
#     Power                       2147.0      Watts
#     Current                     8.55        Amps
#     Power Factor                0.504
#     Volumetric Efficiency       1.003
#     Motor Speed                 3600        RPM



##############################################################################
#Notes about refprope
##############################################################################

#Thermo/Refrigeration Cycle in R - refprope is your friend!
#refprope(prop_req, spec1, value1, spec2, value2, substance)
#Units are English units.

#prop_req	  The refrigerant property to be calculated.
#spec1		  The 1st independant property type.
#value1		  Corresponding independant property value.
#spec2	  	The 2nd independant property type.
#value2 		Corresponding independant property value.
#substance	Refrigerant to be used.

#T - Temperature; P - Pressure; H - Enthalpy; D - Density; C - ; R - ; M - ; S - Entropy; U - ; Q - Quality

##############################################################################
#Calculations
##############################################################################
##############################################################################
#Set known variables
##############################################################################

P = 2147.0   #Power (Watts)
I = 8.55     #Current (Amps)
Volt = 287.3    #Voltage (Volts)
theta = 3    #Phase

PEvap = 70.00  #Pressure during evaporation (psia)
PCond = 217.11 #Pressure during condensation (psia)

TEvap = 20.0 #Evaporation temperature (Fahrenheit)
TCond = 90.0 #Condenstaion temperature (Fahrenheit)
SH = 45.0 #Amount of superheating (Fahrenheit)
SC = 0.0 #Amount of subcooling (Fahrenheit)

D = 2.8        #Displacement
V = 3600       #Motor speed (rpm)
m_dot_evap_min = 468.99/60  #Mass flow (lbm/min)
m_dot_evap = 468.99 #Mass flow (lbm/hr)

##############################################################################
#Calculate h1 and h4
##############################################################################

P1 = PEvap
P2 = PCond
P3 = P2
P4 = P1

T1 = TEvap + SH
T3 = TCond - SC
T4 = TEvap

#If no subcooling occurs, quality = 0 after condensation and before throttling
if (SC == 0.0)
{
  Q3 = 0   #Quality
}

#h1 calculation
#h1 = Enthalpy entering compressor suction (Btu/lb)
substance = 'R404A.mix'  ## Refrigerant
prop_req = 'H'           ## Enthalpy
spec1 = 'T'              ## Temperature
spec2 = 'P'              ## Pressure
value1 = T1              # Farenheit
value2 = P1              # psia
h1 = refprope(prop_req,spec1,value1,spec2,value2,substance)

#h4 (h4 = h3)
#h4 = Enthalpy entering evaporator (assume = enthalpy of liquid exiting condensor) (Btu/lb)
substance = 'R404A.mix'  ## Refrigerant
prop_req = 'H'           ## Enthalpy
spec1 = 'P'              ## Pressure
spec2 = 'Q'              ## Quality
value1 = P3              # psia
value2 = Q3          
h4 = refprope(prop_req,spec1,value1,spec2,value2,substance)

##############################################################################
#Calculate Volumetric Efficiency
##############################################################################

#rho_suction
substance = 'R404A.mix'  ## Refrigerant
prop_req = 'D'           ## Density
spec1 = 'T'              ## Temperature
spec2 = 'P'              ## Pressure
value1 = T1              # Farenheit
value2 = P1              # psia
rho_suction = refprope(prop_req,spec1,value1,spec2,value2,substance)

eta_V = m_dot_evap_min/(rho_suction*D*V)
#m_dot = mass flow through evaporator (lb/hr)
#rho = Density at compressor suction (lb/in^3)
#D = displacement of compressor (in^3/rev)
#V = Compressor Speed (RPM) ??? mismatched units ???

##############################################################################
#Isentropic Efficiency
##############################################################################

#S1
substance = 'R404A.mix'  ## Refrigerant
prop_req = 'S'           ## Entropy
spec1 = 'T'              ## Temperature
spec2 = 'P'              ## Pressure
value1 = T1              # Farenheit
value2 = P1              # psia
s1 = refprope(prop_req,spec1,value1,spec2,value2,substance)
s2 = s1

#H2S
substance = 'R404A.mix'  ## Refrigerant
prop_req = 'H'           ## Density
spec1 = 'P'              ## Pressure
spec2 = 'S'              ## Entropy
value1 = P2              # Farenheit
value2 = s2              # psia
h2s = refprope(prop_req,spec1,value1,spec2,value2,substance)

#Capcity, Btu/hr
Q_evap = m_dot_evap*(h1-h4) #28068.0
#m_dot = mass flow thru evaporator (lb/hr)

#EER, Btu/(hr*Watt)
EER = Q_evap/P #Energy Efficiency
#Q_evap = Capacity (Btu/hr)
#P = Compressor Power (Watts)

TEER = (h1 - h4)/((h2s-h1)/3.413)  #Theoretical Energy Efficiency

eta_I = EER/TEER #Isentropic efficiency
#h1 = Enthalpy entering compressor suction (Btu/lb)
#h4 = Enthalpy entering evaporator (assume = enthalpy of liquid exiting condensor) (Btu/lb)
#h2s = Theoretical enthalpy at discharge pressure assuming that entropy, s, is unchanged during compression. 
#h2s = hs(s1,p2)

##############################################################################
#Calculate Power Factor
##############################################################################

PF = P/(I*Volt*sqrt(theta))  #Power factor
##############################################################################
#Print Important Values
##############################################################################

print("Volumetric Efficiency")
print(eta_V)

print("Isentropic Efficiency")
print(eta_I)

print("Power Factor", quote = FALSE)
print(PF, quote = FALSE)
