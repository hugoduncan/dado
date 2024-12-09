# Clojure Style Guide

## Naming Conventions

### Functions and Variables
- Use complete, unabbreviated names that clearly convey purpose
- Follow `kebab-case` naming convention
- Examples:

```clojure
  ;; Good
  (def customer-orders [])
  (defn calculate-total-price [items]
    ;; implementation
    )

  ;; Avoid
  (def cust-ords [])
  (defn calc-tot-price [items]
    ;; implementation
    )
```

### Functions

Avoid `get-` and `set-` prefixes.

Side-effecting functions should have names ending in a `!`.

### Namespaces
- Use descriptive names that reflect the purpose of the namespace
- Include comprehensive namespace documentation
- Structure:
```clojure
  (ns my-project.core
    "This namespace handles the core business logic for the order processing system.
     It provides functions for order validation, price calculation, and fulfillment."
    (:require
	  [other.namespace :as other]
	  [clojure.string :as str]))
```

### Malli schemas
- Use capitalized names, as you would for `defrecord`,

Examples

``` clojure
(def RoleContent
  [:map
   [:role Role]
   [:content string?]])
```

## Documentation

### Docstrings
- Every public function should have a docstring
- Include:
  - Concise description of purpose
  - Parameter descriptions
  - Return value description
  - Example usage when helpful
- Format:
```clojure
  (defn process-order
    "Processes a customer order and returns updated inventory status.

     Parameters:
       order-items - Vector of maps containing :item-id and :quantity
       inventory   - Current inventory state map

     Returns:
       Updated inventory map with processed quantities

     Example:
       (process-order [{:item-id \"A123\" :quantity 2}] current-inventory)"
    [order-items inventory]
    ;; implementation
    )
```

## Namespaces

The namespace should have a doc string describing its purpose.

- in the `:require` form
  - Check that all namespaces are actually used.
  - Order the required namespaces alphabetically.

## Function Design

### Argument order

Order arguments from most general to most specific.

Maintain the relative order of arguments across all functions.


### Cyclomatic Complexity
- Keep functions simple with low cyclomatic complexity
- Guidelines:
  - Single responsibility principle
  - Maximum of 2-3 levels of nesting
  - Prefer function composition over complex conditionals
  - Extract complex logic into smaller, focused functions

Example of reducing complexity:
```clojure
;; Avoid
(defn process-customer-data [customer]
  (if (valid? customer)
    (if (premium? customer)
      (if (has-pending-orders? customer)
        (process-premium-with-pending customer)
        (process-premium-without-pending customer))
      (process-regular-customer customer))
    (throw (ex-info "Invalid customer" {:customer customer}))))

;; Better
(defn process-customer-data
  "Process customer data based on their status and pending orders.
   Throws ex-info if customer is invalid."
  [customer]
  (when-not (valid? customer)
    (throw (ex-info "Invalid customer" {:customer customer})))

  (if (premium? customer)
    (process-premium-customer customer)
    (process-regular-customer customer)))

(defn process-premium-customer
  "Handle processing for premium customers based on pending order status."
  [customer]
  (if (has-pending-orders? customer)
    (process-premium-with-pending customer)
    (process-premium-without-pending customer)))
```

## Data Structure Usage

### Collections
- Prefer vectors `[]` for sequences with random access
- Use lists `()` for sequences that are processed sequentially
- Use sets `#{}` for unique collections
- Use maps `{}` for key-value associations

### Threading Macros
- Use thread-first `->` when transforming a value through functions that take it as first argument
- Use thread-last `->>` when transforming a value through functions that take it as last argument

Example:

```clojure
;; Using thread-first for clear data transformation
(defn prepare-customer-name
  "Formats customer name according to business rules."
  [customer-name]
  (-> customer-name
      str/trim
      str/lower-case
      capitalize-words
      remove-special-characters))

;; Using thread-last for collection processing
(defn process-orders
  "Process all orders and return successfully processed ones."
  [orders]
  (->> orders
       (filter valid-order?)
       (map process-single-order)
       (remove :errors)))
```

## Testing

### Test Organization
- Tests should mirror the namespace structure
- Use descriptive test names
- Include both positive and negative test cases
- Example:

```clojure
  (ns my-project.core-test
    "Tests for core order processing functionality."
    (:require [clojure.test :refer :all]
              [my-project.core :as core]))

  (deftest process-order-test
    (testing "successful order processing"
      (let [result (core/process-order valid-order)]
        (is (= :success (:status result)))))

    (testing "handling invalid orders"
      (is (thrown? ExceptionInfo
                   (core/process-order invalid-order)))))
```

## Error Handling

### Best Practices
- Use `ex-info` for custom exceptions with structured data
- Prefer condition systems over raw try/catch where appropriate
- Validate input data early
- Example:

```clojure
  (defn process-payment
    "Process payment for an order.
     Throws ex-info if payment validation fails."
    [payment-data]
    (when-not (valid-payment? payment-data)
      (throw (ex-info "Invalid payment data"
                     {:type :validation-error
                      :data payment-data})))
    ;; Process payment
    )
```

## Formatting

- Do NOT uses tabs, always uses spaces
