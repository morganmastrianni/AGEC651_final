using XLSX, DataFrames, Dates

cd(pwd()*"\\Final_project")

data = XLSX.readdata("icg_goi_indices_only.xlsx", "Sheet1", "A1:G1064")

dims = size(data)
pre_dims = 

dims = size(data)
date = Date.(data[2:dims[1],1])
invasion_date = Date(2022, 02, 24)
invasion_date_index = findall(date .== invasion_date)
invasion_date_index = invasion_date_index[1]

wheat = Float64.(data[2:dims[1],3])
wheat_pre = wheat[1:invasion_date_index]
maize = Float64.(data[2:dims[1],4])
maize_pre = maize[1:invasion_date_index]
soy = Float64.(data[2:dims[1],5])
soy_pre = soy[1:invasion_date_index]
rice = Float64.(data[2:dims[1],6])
rice_pre = rice[1:invasion_date_index]
barley = Float64.(data[2:dims[1],7])
barley_pre = barley[1:invasion_date_index]

# weights are tested on pre data
k = invasion_date_index
j = dims[2]-3

W = zeros(Float64, j, 1)
X_1 = zeros(Float64, k, 1)
X_0 = zeros(Float64, k, j)

commodities = hcat(maize, soy, rice, barley)
commodities_pre = hcat(maize_pre,soy_pre,rice_pre,barley_pre)

W = inv(commodities_pre'*commodities_pre)*commodities_pre'*wheat_pre
# add up to 1?
sum(W) # pretty close... how to impose condition?

# test weights: apply to commodities
synth_wheat = commodities_pre*W

# visualize
using Plots
plot(date, wheat, label="wheat")
plot!(date, maize, label="maize")
plot!(date, soy, label="soy")
plot!(date, rice, label="rice")
plot!(date, barley, label="barley")

plot(date[1:length(wheat_pre)-1], wheat_pre[1:length(wheat_pre)-1], label="wheat")
plot!(date[1:length(wheat_pre)-1], synth_wheat[1:length(wheat_pre)-1], label="synth_wheat")

# compare to wheat
diffs = synth_wheat-wheat_pre

using Statistics
mean(diffs)

# really i dont want a commodity that resembles wheat in particular...
# want something that most closely tracks its volatility

# create vectors for % change in daily prices
d_w = Float64[]
for i in 2:length(wheat)
    delta = wheat[i]/wheat[i-1]
    push!(d_w, delta)
end

#d_c = zeros(Float64, dims[1]-2, j)

#for i in 2:size(d_c)[1]+1
#    delta_m = maize[i]/maize[i-1]
#    d_c[i-1,1] = delta_m
#    delta_s = soy[i]/soy[i-1]
#    d_c[i-1,2] - delta_s
#    delta_r = rice[i]/rice[i-1]
#    d_c[i-1,3] = delta_r
#    delta_b = barley[i]/barley[i-1]
#    d_c[i-1,4] - delta_b
#end

# all of the stuff below shoudld fit pretty neatly into a for loop
# ill work on that later
# should i be using ln(price_day2/price_day1)?

d_m = Float64[]
for i in 2:length(maize)
    delta = maize[i]/maize[i-1]
    push!(d_m, delta)
end

d_s = Float64[]
for i in 2:length(soy)
    delta = soy[i]/soy[i-1]
    push!(d_s, delta)
end

d_r = Float64[]
for i in 2:length(rice)
    delta = rice[i]/rice[i-1]
    push!(d_r, delta)
end

d_b = Float64[]
for i in 2:length(barley)
    delta = barley[i]/barley[i-1]
    push!(d_b, delta)
end

# 1st index of each vector is change in price between day 2 and day 1
# pre-invasion index for change in price will be invasion_date_index-1 (day before invasion)
# weights are tested on pre data
k_d = invasion_date_index-1

d_w_pre = d_w[1:k_d]
d_m_pre = d_m[1:k_d]
d_s_pre = d_s[1:k_d]
d_r_pre = d_r[1:k_d]
d_b_pre = d_b[1:k_d]

W_d = zeros(Float64, j, 1)
X_1_d = zeros(Float64, k_d, 1)
X_0_d = zeros(Float64, k_d, j)

c_pre_d = hcat(d_m_pre,d_s_pre,d_r_pre,d_b_pre)

W_d = inv(c_pre_d'*c_pre_d)*c_pre_d'*d_w_pre
# add up to 1?
sum(W_d) # REALLY close...

# test weights: apply to commodities
s_d_w = c_pre_d*W_d

# compare to wheat
d_diffs = s_d_w-d_w_pre # pretty darn close to 0

# visualize
plot(date[2:length(date)], d_w, label="d_wheat")
plot!(date[2:length(date)], d_m, label="d_maize")
plot!(date[2:length(date)], d_s, label="d_soy")
plot!(date[2:length(date)], d_r, label="d_rice")
plot!(date[2:length(date)], d_b, label="d_barley")


# NOW... we can use weights to forecast the commodity bundle over the whole time period
wheat_forecast = commodities*W_d # not a forecast of actual wheat
# a forecast of wheat changes in price

# plot
plot(date, wheat, label="wheat")
plot!(date, wheat_forecast, label="wheat_forecast")

# average price ratio in pre period
rps_pre = Float64[]
for i in 1:invasion_date_index-1
    ratio_ps = wheat[i]/wheat_forecast[i]
    push!(rps_pre, ratio_ps)
end
avg_rps_pre = mean(rps_pre)

rps_post = Float64[]
for i in invasion_date_index:length(wheat)
    ratio_ps = wheat[i]/wheat_forecast[i]
    push!(rps_post, ratio_ps)
end
avg_rps_post = mean(rps_post)

rps = vcat(rps_pre, rps_post)
plot(date, rps)

# minimize hurst coeff over x # of regimes; find regime breaks that min h coeffs



