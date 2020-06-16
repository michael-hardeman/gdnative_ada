extends Control

# load the Simple library
onready var simple = preload("res://lib/simple.gdns").new()

func _on_Button_pressed():
	$Label.text = "Data = " + simple.get_data()
