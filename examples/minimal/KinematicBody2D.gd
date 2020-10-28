extends KinematicBody2D


# Declare member variables here. Examples:
# var a = 2
# var b = "text"


# Called when the node enters the scene tree for the first time.
func _ready():
	pass # Replace with function body.


func _physics_process(delta):
	var velocity = Vector2(0, 1000);
	self.move_and_slide(velocity, Vector2(0, 1))

# Called every frame. 'delta' is the elapsed time since the previous frame.
#func _process(delta):
#	pass
