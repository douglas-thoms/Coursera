# dict.regex <- dictionary(list(at.mark = "[@]",
#                         number = "[0-9]",
#                         period = "[.]"
# ))
# 
# z <- dfm(tokens_select(total.tokens, dict, selection = "keep", valuetype = "regex"))

#www.
#.com
#.gov
#.net


dict.fix <- dictionary(list(profanity = profanity$V1
))

z <- dfm(tokens_select(total.tokens, dict.fix, selection = "keep", valuetype = "fixed"))