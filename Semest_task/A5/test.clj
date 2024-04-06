{inc (-> :Int :Int)}
{inc (-> :Int :Int), x :Str}
{inc (-> :Int :Int)}
{inc (-> :Int :Int), >0 (-> :Int :Bool)}
{inc (-> :Int :Int)}
{= (-> A (-> A :Bool))}
{= (-> A (-> A :Bool))}
(lambda x (inc x))
((lambda x x) 10)
[(concat x) (lambda x (inc (inc x)))]
{head (-> (:List A) A), x (:List :Int)}