module TinyJOS

type AbstractClass

# Stadnard instance

type StdInstance
	class :: AbstractClass
	slots :: Any
end

is_instance_slot(slot) = slot_definition_allocation(slot) === :instance

std_instance_class(x :: StdInstance) = x.class
std_instance_slots(x :: StdInstance) = x.slots

const secrect_unbound_value = Symbol('unbound slot value')

allocate_std_instance(class) =
	StdInstance(
		class,
		allocate_slot_storage(
			count(is_instance_slot, class_slots(class),
			secrect_unbound_value)))

allocate_slot_storage(num, init) = fill(init, num)

the_slots_of_standard_class = Nullable()
the_class_standard_class = Nullable()

function slot_location(class, slot_name)
	if (slot_name === :effective_slots) && (class === the_class_standard_class)
		findfirst(s -> slot_definition_name(s) === :effective_slots, the_slots_of_standard_class)
	else
		pos = findfirst(s -> slot_definition_name(s) === slot_name, class_slots(class))
		if pos == 0
			error("Slot $slot_name not found in class $class")
		end
		slot = class_slots(class)[pos]
		instpos = findfirst(
			s -> slot_definition_name(s) === slot_name,
			filter(is_instance_slot, class_slots(class)))
		if instpos == 0
			error("Slot $slot_name is not an instance slot of class $class")
		end
		instpos
end

# Not sure if needed: can do with getindex and setindex! on slots type
slot_contents(slots, loc) = slots[loc]

set_slot_contents!(slots, loc, newval) = slots[loc] = newval

function std_slot_value(instance, slot_name)
	loc = slot_location(classof(instance), slot_name)
	slots = std_instance_slots(instance)
	val = slots[loc]
	if val === secrect_unbound_value
		error("Slot $slot_name is unbound in $instance")
	end
	val
end

function slot_value(instance, slot_name)
	if classof(classof(instance)) === the_class_standard_class
		std_slot_value(instance, slot_name)
	else
		slot_value_using_class(classof(instance), instance, slot_name)
	end
end

function set_std_slot_value!(instance, slot_name, newval)
	loc = slot_location((classof instance), slot_name)
	slots = std_instance_slots(instance)
	slots[loc] = newval
end

function set_slot_value!(instance, slot_name, newval)
	if classof(classof(instance)) === the_class_standard_class
		set_std_slot_value!(instance, slot_name, newval)
	else
		set_slot_value_using_class!(classof(instance), instance, slot_name, newval)
	end
end

function is_std_slot_bound(instance, slot_name)
	loc = slot_location(classof(instance), slot_name)
	slots = std_instance_slots(instance)
	stlos[loc] != secrect_unbound_value
end

function is_slot_bound(instance, slot_name)
	if classof(classof(instnace)) === the_class_standard_class
		is_std_slot_bound(instance, slot_name)
	else
		is_slot_bound_using_class(classof(instance), instnace, slot_name)
	end
end

function std_slot_make_unbound(instance, slot_name)
	loc = slot_location(classof(instnace), slot_name)
	slots = std_instance_slots(instance)
	slots[loc] = secrect_unbound_value
	nothing
end


function slot_make_unbound(instance, slot_name)
	if classof(classof(instnace)) === the_class_standard_class
		std_slot_make_unbound(instance, slot_name)
	else
		slot_make_unbound_using_class(classof(instance), instance, slot_name)
	end
end

function is_std_slot_exists(instance, slot_name)
	findfirst(
		s -> slot_definition_name(s) === slot_name,
		class_slots(classof(instance))) > 0
end

function is_slot_exists(instance, slot_name)
	if classof(classof(instance)) === the_class_standard_class
		is_std_slot_exists(instance, slot_name)
	else
		is_slot_exists_using_class(classof(instance), instance, slot_name)
	end
end

function classof(x)
	if typeof(x) == StdInstance
		std_instance_class(x)
	else
		built_in_class(x)
	end
end

function built_in_class(x)
	if typeof(x) == Bool
		findclass(:Bool)
	elseif typeof(x) == Int64
		findclass(:Int64)
	elseif typeof(x) == Float64
		findclass(:Float64)
	elseif typeof(x) == Symbol
		findclass(:Symbol)
	elseif typeof(x) == Char
		findclass(:Char)
	elseif typeof(x) == String
		findclass(:String)
	elseif typeof(x) <: Function
		findclass(:Function)
	else
		error("Not implemented for $(typeof(x))")
	end
end

function issubclass(c1, c2)
	findfirst(x -> x == c2, class_precedence_list(c1)) > 0
end

function is_subspecializer(c1, c2, cmain)
	cpl = class_precedence_list(cmain)
	c1loc = findfirst(x -> x == c1, cpl)
	if c1loc <= 0
		return false
	end
	findfirst(x -> x == c2, cpl[c1loc:end]) > 0
end



end # module
