res[loss_type == "maha", pval := pchisq(loss, p-1, lower.tail = F)]
res[loss_type == "maha3", pval := pchisq(loss, p-d, lower.tail = F)]
res[loss_type == "maha2", pval := pchisq(loss, d-1, lower.tail = F)]
