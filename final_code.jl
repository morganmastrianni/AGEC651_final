using XLSX
using DataFrames

cd(pwd()*"\\Final_project")
#using ExcelReaders
#using ExcelFiles
XLSX.openxlsx("igc__goi.xlsx", enable_cache=false) do f
    #global data = f["GOI & Indices"]
    data = f["GOI & Indices"]
    for r in XLSX.eachrow(data)
        global row_data = collect(r)
        if !any(ismissing, r)
            #rn = XLSX.row_number(r)
            global v1 = r[1]
            global v2 = r[2]
            global v3 = r[3]
            println("v1=$v1, v2=$v2, v3=$v3")
        end
    end
end

###
using Dates
v1 = Date[]
v2 = String[]
v3 = String[]

XLSX.openxlsx("igc__goi.xlsx", enable_cache=false) do f
    #global data = f["GOI & Indices"]
    data = f["GOI & Indices"]
    for r in XLSX.eachrow(data)
        #global row_data = collect(r)
            #rn = XLSX.row_number(r)
            v_1 = r[1]
            if v_1 == "missing"
                push!(v1, Date("missing"))
            else
                push!(v1, coalesce(Date(v_1, "yyyy-mm-dd"), Date("missing")))
            end
            push!(v1, v_1)
            #global v2 = coalesce(r[2], 0)
            #global v3 = coalesce(r[3], 0)
            #println("v1=$v1, v2=$v2, v3=$v3")
        #end
    end
end
###

file = XLSX.openxlsx("igc__goi.xlsx", enable_cache=false) do f
    data = file["GOI & Indices"]


